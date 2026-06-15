package screach;

import docking.ActionContext;
import docking.ComponentProvider;
import docking.action.DockingAction;
import docking.action.ToolBarData;

import ghidra.app.services.GoToService;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;
import ghidra.util.HelpLocation;
import ghidra.util.Msg;

import resources.Icons;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Font;
import java.awt.Toolkit;
import java.awt.datatransfer.StringSelection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;

/**
 * Docked window that shows the Screach command, its streamed output, and a colored banner
 * reflecting the outcome. Local toolbar actions let the user clear the log, navigate to the last
 * address Screach reported, and find the coverage file for loading into grease-cartographer.
 */
class ScreachProvider extends ComponentProvider implements ScreachState.ChangeListener {

    /** Cap the log so a verbose ({@code -vvv}) run cannot exhaust memory. */
    private static final int MAX_LOG_CHARS = 2_000_000;

    private static final Pattern HEX = Pattern.compile("\\b0x([0-9a-fA-F]+)\\b");

    private final ScreachPlugin plugin;
    private final JPanel panel;
    private final JLabel banner;
    private final JTextArea log;

    private volatile String lastCoverageFile;

    ScreachProvider(ScreachPlugin plugin) {
        super(plugin.getTool(), "Screach", plugin.getName());
        this.plugin = plugin;

        panel = new JPanel(new BorderLayout());
        banner = new JLabel("Screach: set an entry and target address, then run.");
        banner.setOpaque(true);
        banner.setBorder(BorderFactory.createEmptyBorder(4, 8, 4, 8));
        setBannerColors(Color.LIGHT_GRAY);

        log = new JTextArea(20, 80);
        log.setEditable(false);
        log.setFont(new Font(Font.MONOSPACED, Font.PLAIN, 12));

        panel.add(banner, BorderLayout.NORTH);
        panel.add(new JScrollPane(log), BorderLayout.CENTER);

        // [ref:screach_help_results]
        setHelpLocation(new HelpLocation(ScreachPlugin.HELP_TOPIC, "results"));
        setDefaultWindowPosition(docking.WindowPosition.BOTTOM);

        plugin.getState().setChangeListener(this);
        createActions();
    }

    private void createActions() {
        DockingAction clear =
                new DockingAction("Clear Screach Output", getOwner()) {
                    @Override
                    public void actionPerformed(ActionContext context) {
                        log.setText("");
                    }
                };
        clear.setToolBarData(new ToolBarData(Icons.CLEAR_ICON, null));
        clear.markHelpUnnecessary();
        clear.setEnabled(true);
        addLocalAction(clear);

        DockingAction goTo =
                new DockingAction("Go To Last Screach Address", getOwner()) {
                    @Override
                    public void actionPerformed(ActionContext context) {
                        goToLastAddress();
                    }
                };
        goTo.setToolBarData(new ToolBarData(Icons.NAVIGATE_ON_INCOMING_EVENT_ICON, null));
        goTo.markHelpUnnecessary();
        goTo.setEnabled(true);
        addLocalAction(goTo);

        DockingAction coverage =
                new DockingAction("Load Screach Coverage", getOwner()) {
                    @Override
                    public void actionPerformed(ActionContext context) {
                        showCoverageInstructions();
                    }
                };
        coverage.setToolBarData(new ToolBarData(Icons.OPEN_FOLDER_ICON, null));
        coverage.markHelpUnnecessary();
        coverage.setEnabled(true);
        addLocalAction(coverage);
    }

    @Override
    public JComponent getComponent() {
        return panel;
    }

    // --- state listener -----------------------------------------------------

    @Override
    public void stateChanged(ScreachState state) {
        // Keep the banner in sync only while no run is in progress / shown.
        if (!state.isReadyToRun()) {
            SwingUtilities.invokeLater(
                    () -> {
                        if (state.getEntry() == null && state.getTarget() == null) {
                            setBanner(
                                    "Screach: set an entry and target address, then run.",
                                    Color.LIGHT_GRAY);
                        }
                    });
        }
    }

    void reportAddressSet(String role, Program program, Address ghidraAddress) {
        String screach = AddressTranslator.formatForCli(program, ghidraAddress);
        append("[screach] " + role + ": ghidra=" + ghidraAddress + " -> screach=" + screach + "\n");
        setVisible(true);
    }

    void reportAvoidCleared() {
        append("[screach] cleared all avoid addresses\n");
        setVisible(true);
    }

    // --- run lifecycle (may be called off the Swing thread) -----------------

    void startRun(String command) {
        lastCoverageFile = null;
        SwingUtilities.invokeLater(
                () -> {
                    setBanner("Running Screach…", new Color(210, 210, 210));
                    log.append("\n$ " + command + "\n");
                });
    }

    void appendStdout(String text) {
        append(text);
    }

    void appendStderr(String text) {
        append(text);
    }

    void finishRun(ScreachResult result) {
        lastCoverageFile = result.coverageFile;
        SwingUtilities.invokeLater(
                () -> {
                    switch (result.status) {
                        case REACHED:
                            setBanner("Reached target!", new Color(180, 230, 180));
                            break;
                        case NOT_REACHED:
                            setBanner("Target not reached.", new Color(245, 220, 160));
                            break;
                        case CANCELLED:
                            setBanner("Run cancelled.", Color.LIGHT_GRAY);
                            break;
                        case ERROR:
                        default:
                            setBanner(result.summary, new Color(240, 180, 180));
                            break;
                    }
                    log.append("[screach] " + result.summary + "\n");
                    if (result.coverageFile != null) {
                        log.append("[screach] Coverage written to: " + result.coverageFile + "\n");
                        log.append(
                                "[screach] Load it via Tools → Code Coverage → "
                                        + "Load Code Coverage File(s)… (grease-cartographer).\n");
                    }
                });
    }

    // --- helpers ------------------------------------------------------------

    private void goToLastAddress() {
        GoToService goToService = plugin.getGoToService();
        Program program = plugin.getCurrentProgram();
        if (goToService == null || program == null) {
            return;
        }
        // Find the last hex literal in the log and navigate to it.
        Matcher m = HEX.matcher(log.getText());
        String last = null;
        while (m.find()) {
            last = m.group(1);
        }
        if (last == null || last.length() > 16) {
            Msg.showInfo(this, panel, "Screach", "No address found in the output.");
            return;
        }
        long screachOffset = Long.parseUnsignedLong(last, 16);
        Address addr = AddressTranslator.screachToGhidra(program, screachOffset);
        if (addr != null) {
            goToService.goTo(addr);
        } else {
            Msg.showInfo(
                    this,
                    panel,
                    "Screach",
                    "0x" + last + " is not within this program's address space.");
        }
    }

    private void showCoverageInstructions() {
        if (lastCoverageFile == null) {
            Msg.showInfo(
                    this,
                    panel,
                    "Screach Coverage",
                    "No coverage file from the last run. Enable 'Dump Coverage' in "
                            + "Edit → Tool Options → Screach and run again.");
            return;
        }
        Toolkit.getDefaultToolkit()
                .getSystemClipboard()
                .setContents(new StringSelection(lastCoverageFile), null);
        Msg.showInfo(
                this,
                panel,
                "Screach Coverage",
                "Coverage file (path copied to clipboard):\n"
                        + lastCoverageFile
                        + "\n\nLoad it in grease-cartographer via:\n"
                        + "Tools → Code Coverage → Load Code Coverage File(s)…");
    }

    private void append(String text) {
        SwingUtilities.invokeLater(
                () -> {
                    log.append(text);
                    int len = log.getDocument().getLength();
                    if (len > MAX_LOG_CHARS) {
                        log.replaceRange("", 0, len - MAX_LOG_CHARS);
                    }
                    log.setCaretPosition(log.getDocument().getLength());
                });
    }

    private void setBanner(String text, Color color) {
        banner.setText(text);
        setBannerColors(color);
    }

    private void setBannerColors(Color background) {
        banner.setBackground(background);
        banner.setForeground(Color.BLACK);
    }

    void dispose() {
        plugin.getState().setChangeListener(null);
        removeFromTool();
    }
}

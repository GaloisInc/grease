package screach;

import docking.ActionContext;
import docking.action.DockingAction;
import docking.action.KeyBindingData;
import docking.action.MenuData;

import ghidra.program.model.listing.Program;
import ghidra.util.HelpLocation;
import ghidra.util.Msg;
import ghidra.util.task.TaskLauncher;

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.io.File;

/**
 * Menu item (top-level <i>Screach</i> menu) and keyboard shortcut (Ctrl+Alt+R) that runs Screach
 * using the currently-set entry and target addresses.
 */
class RunScreachAction extends DockingAction {

    private final ScreachPlugin plugin;

    RunScreachAction(ScreachPlugin plugin) {
        super("Run Screach", plugin.getName());
        this.plugin = plugin;
        // [tag:screach_menu_run]
        setMenuBarData(
                new MenuData(new String[] {"Screach", "Run Screach"}, ScreachRole.MENU_GROUP));
        setKeyBindingData(
                new KeyBindingData(
                        KeyEvent.VK_R, InputEvent.CTRL_DOWN_MASK | InputEvent.ALT_DOWN_MASK));
        setHelpLocation(
                new HelpLocation(ScreachPlugin.HELP_TOPIC, "run")); // [ref:screach_help_run]
        setEnabled(true);
    }

    @Override
    public boolean isEnabledForContext(ActionContext context) {
        return plugin.getCurrentProgram() != null && plugin.getState().isReadyToRun();
    }

    @Override
    public void actionPerformed(ActionContext context) {
        Program program = plugin.getCurrentProgram();
        if (program == null) {
            Msg.showWarn(this, null, "Screach", "Open a program first.");
            return;
        }
        ScreachState state = plugin.getState();
        if (!state.isReadyToRun()) {
            Msg.showWarn(
                    this,
                    null,
                    "Screach",
                    "Set both a Screach entry and target address first (right-click in the"
                            + " Listing).");
            return;
        }

        String programPath = program.getExecutablePath();
        if (programPath == null || programPath.isBlank() || !new File(programPath).isFile()) {
            Msg.showError(
                    this,
                    null,
                    "Screach",
                    "The program's on-disk file could not be found ("
                            + programPath
                            + "). Screach "
                            + "needs the original binary to analyze.");
            return;
        }

        plugin.getProvider().setVisible(true);

        ScreachRunnerTask task = new ScreachRunnerTask(plugin, program, state.snapshot());
        TaskLauncher.launch(task);
    }
}

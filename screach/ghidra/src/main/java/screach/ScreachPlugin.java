package screach;

import ghidra.MiscellaneousPluginPackage;
import ghidra.app.plugin.PluginCategoryNames;
import ghidra.app.plugin.ProgramPlugin;
import ghidra.app.services.GoToService;
import ghidra.framework.options.SaveState;
import ghidra.framework.plugintool.PluginInfo;
import ghidra.framework.plugintool.PluginTool;
import ghidra.framework.plugintool.util.PluginStatus;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;

/**
 * Drives the external <a href="https://github.com/GaloisInc/grease">Screach</a> reachability
 * analyzer from within Ghidra.
 *
 * <p>The plugin lets an analyst right-click in the Listing to mark an entry (source) address, a
 * target address, and any number of addresses to avoid, then run Screach (as a local binary or via
 * a Docker image) from a menu item or keyboard shortcut. Results are shown in a docked window. When
 * coverage dumping is enabled, Screach writes a {@code .greasecov} file that can be loaded into the
 * grease-cartographer coverage heatmap plugin.
 */
// @formatter:off
@PluginInfo(
        status = PluginStatus.UNSTABLE,
        packageName = MiscellaneousPluginPackage.NAME,
        category = PluginCategoryNames.ANALYSIS,
        shortDescription = "Drive the Screach reachability tool from Ghidra.",
        description =
                "Set entry/target/avoid addresses by right-clicking in the Listing, run the Screach"
                    + " reachability analyzer (binary or Docker), view results in a docked window,"
                    + " and optionally produce a coverage file for the grease-cartographer heatmap"
                    + " plugin.")
// @formatter:on
public class ScreachPlugin extends ProgramPlugin {

    /** Help topic name; shared by every {@code HelpLocation} in this module. */
    public static final String HELP_TOPIC = "screach";

    private static final String STATE_ENTRY = "screach.entry";
    private static final String STATE_TARGET = "screach.target";
    private static final String STATE_AVOID = "screach.avoid";

    private final ScreachState state = new ScreachState();
    private final ScreachOptions options;
    private ScreachProvider provider;
    private GoToService goToService;

    public ScreachPlugin(PluginTool tool) {
        super(tool);
        options = new ScreachOptions(tool);
        provider = new ScreachProvider(this);

        createActions();
    }

    @Override
    protected void init() {
        super.init();
        goToService = tool.getService(GoToService.class);
    }

    @Override
    public void dispose() {
        if (provider != null) {
            provider.dispose();
        }
        super.dispose();
    }

    private void createActions() {
        for (ScreachRole role : ScreachRole.values()) {
            tool.addAction(new ListingScreachAction(this, role));
            tool.addAction(new FunctionScreachAction(this, role));
        }
        tool.addAction(new RunScreachAction(this));
        tool.addAction(new ClearAvoidScreachAction(this));
    }

    ScreachState getState() {
        return state;
    }

    ScreachOptions getOptions() {
        return options;
    }

    ScreachProvider getProvider() {
        return provider;
    }

    GoToService getGoToService() {
        return goToService;
    }

    @Override
    protected void programDeactivated(Program program) {
        // Addresses belong to a specific program; clear them when it closes so we
        // never hand a stale address to Screach.
        state.clear();
    }

    @Override
    public void writeConfigState(SaveState saveState) {
        Address entry = state.getEntry();
        Address target = state.getTarget();
        if (entry != null) {
            saveState.putLong(STATE_ENTRY, entry.getOffset());
        }
        if (target != null) {
            saveState.putLong(STATE_TARGET, target.getOffset());
        }
        long[] avoid = state.getAvoid().stream().mapToLong(Address::getOffset).toArray();
        saveState.putLongs(STATE_AVOID, avoid);
    }

    @Override
    public void readConfigState(SaveState saveState) {
        Program program = getCurrentProgram();
        if (program == null) {
            return;
        }
        if (saveState.hasValue(STATE_ENTRY)) {
            state.setEntry(toAddress(program, saveState.getLong(STATE_ENTRY, 0)));
        }
        if (saveState.hasValue(STATE_TARGET)) {
            state.setTarget(toAddress(program, saveState.getLong(STATE_TARGET, 0)));
        }
        long[] avoid = saveState.getLongs(STATE_AVOID, new long[0]);
        for (long off : avoid) {
            state.addAvoid(toAddress(program, off));
        }
    }

    private static Address toAddress(Program program, long offset) {
        return program.getAddressFactory().getDefaultAddressSpace().getAddress(offset);
    }
}

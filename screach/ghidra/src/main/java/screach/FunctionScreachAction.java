package screach;

import docking.ActionContext;
import docking.action.DockingAction;
import docking.action.MenuData;

import ghidra.app.context.FunctionSupplierContext;
import ghidra.app.context.ProgramLocationActionContext;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Function;
import ghidra.program.model.listing.Program;
import ghidra.util.HelpLocation;

import java.util.Set;

/**
 * Right-click action for the Functions window (and any other component whose context supplies
 * functions). Uses each function's entry-point address as the Screach entry/target/avoid, mirroring
 * the {@link ListingScreachAction} items.
 */
class FunctionScreachAction extends DockingAction {

    private final ScreachPlugin plugin;
    private final ScreachRole role;

    FunctionScreachAction(ScreachPlugin plugin, ScreachRole role) {
        super(role.functionActionName, plugin.getName());
        this.plugin = plugin;
        this.role = role;
        setPopupMenuData(
                new MenuData(
                        new String[] {"Screach", role.menuItem}, null, ScreachRole.MENU_GROUP));
        setHelpLocation(new HelpLocation(ScreachPlugin.HELP_TOPIC, role.helpAnchor));
    }

    @Override
    public boolean isAddToPopup(ActionContext context) {
        return isFunctionListContext(context) && ((FunctionSupplierContext) context).hasFunctions();
    }

    @Override
    public boolean isEnabledForContext(ActionContext context) {
        if (!isFunctionListContext(context)) {
            return false;
        }
        FunctionSupplierContext fsc = (FunctionSupplierContext) context;
        if (!fsc.hasFunctions()) {
            return false;
        }
        // Entry and target are single-valued, so only enable them for a single
        // selected function. Avoid can take any number.
        return role.isMultiValued() || fsc.getFunctions().size() == 1;
    }

    /**
     * True for contexts that supply functions but are <em>not</em> the Listing's location context.
     * The Listing's {@code ProgramLocationActionContext} also implements {@link
     * FunctionSupplierContext}, but {@link ListingScreachAction} already handles it; matching here
     * too would produce duplicate popup entries.
     */
    private static boolean isFunctionListContext(ActionContext context) {
        return context instanceof FunctionSupplierContext
                && !(context instanceof ProgramLocationActionContext);
    }

    @Override
    public void actionPerformed(ActionContext context) {
        Set<Function> functions = ((FunctionSupplierContext) context).getFunctions();
        ScreachState state = plugin.getState();
        ScreachProvider provider = plugin.getProvider();
        for (Function function : functions) {
            Program program = function.getProgram();
            Address entry = function.getEntryPoint();
            role.apply(state, entry);
            provider.reportAddressSet(
                    role.reportLabel + " (" + function.getName() + ")", program, entry);
        }
    }
}

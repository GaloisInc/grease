package screach;

import docking.action.MenuData;

import ghidra.app.context.ListingActionContext;
import ghidra.app.context.ListingContextAction;
import ghidra.program.model.address.Address;
import ghidra.program.model.listing.Program;
import ghidra.util.HelpLocation;

/**
 * Right-click action in the Listing that sets the address under the cursor as a Screach
 * entry/target/avoid, according to its {@link ScreachRole}.
 */
class ListingScreachAction extends ListingContextAction {

    private final ScreachPlugin plugin;
    private final ScreachRole role;

    ListingScreachAction(ScreachPlugin plugin, ScreachRole role) {
        super(role.listingActionName, plugin.getName());
        this.plugin = plugin;
        this.role = role;
        setPopupMenuData(
                new MenuData(
                        new String[] {"Screach", role.menuItem}, null, ScreachRole.MENU_GROUP));
        setHelpLocation(new HelpLocation(ScreachPlugin.HELP_TOPIC, role.helpAnchor));
    }

    @Override
    protected boolean isEnabledForContext(ListingActionContext context) {
        return context.getAddress() != null;
    }

    @Override
    protected void actionPerformed(ListingActionContext context) {
        Program program = context.getProgram();
        Address address = context.getAddress();
        role.apply(plugin.getState(), address);
        plugin.getProvider().reportAddressSet(role.reportLabel, program, address);
    }
}

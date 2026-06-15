package screach;

import docking.ActionContext;
import docking.action.DockingAction;
import docking.action.MenuData;

import ghidra.util.HelpLocation;

/**
 * Menu item (top-level <i>Screach</i> menu) that clears all currently-set avoid addresses.
 *
 * <p>Entry and target addresses can each be reset by setting a new one (last selection wins), but
 * avoid addresses accumulate, so this action provides the only way to discard them short of closing
 * the program.
 */
class ClearAvoidScreachAction extends DockingAction {

    private final ScreachPlugin plugin;

    ClearAvoidScreachAction(ScreachPlugin plugin) {
        super("Clear Screach Avoid Addresses", plugin.getName());
        this.plugin = plugin;
        // [tag:screach_menu_clear_avoid]
        setMenuBarData(
                new MenuData(
                        new String[] {"Screach", "Clear Screach Avoid Addresses"},
                        ScreachRole.MENU_GROUP));
        setHelpLocation(
                new HelpLocation(
                        ScreachPlugin.HELP_TOPIC, "clear_avoid")); // [ref:screach_help_clear_avoid]
        setEnabled(true);
    }

    @Override
    public boolean isEnabledForContext(ActionContext context) {
        return !plugin.getState().getAvoid().isEmpty();
    }

    @Override
    public void actionPerformed(ActionContext context) {
        plugin.getState().clearAvoid();
        plugin.getProvider().reportAvoidCleared();
    }
}

package screach;

import ghidra.program.model.address.Address;

import java.util.function.BiConsumer;

/**
 * The three kinds of address input Screach accepts: the entry (source) address, the target address,
 * and any number of addresses to avoid.
 *
 * <p>This enum is the single source of truth for each role's menu text, help anchor, and how its
 * address is applied to {@link ScreachState}. It is shared by the Listing right-click action
 * ({@link ListingScreachAction}) and the Functions-window action ({@link FunctionScreachAction}) so
 * the two cannot drift apart.
 */
enum ScreachRole {
    // [tag:screach_menu_entry]
    ENTRY(
            "Set Screach Entry",
            "Set Screach Entry From Function",
            "Set as Screach Entry (Source) Address",
            "set_entry",
            "entry",
            ScreachState::setEntry),
    // [tag:screach_menu_target]
    TARGET(
            "Set Screach Target",
            "Set Screach Target From Function",
            "Set as Screach Target Address",
            "set_target",
            "target",
            ScreachState::setTarget),
    // [tag:screach_menu_avoid]
    AVOID(
            "Add Screach Avoid",
            "Add Screach Avoid From Function",
            "Add Screach Avoid Address",
            "add_avoid",
            "avoid",
            ScreachState::addAvoid);

    /** Menu group shared by every Screach action. */
    static final String MENU_GROUP = "Screach";

    /** Action name for the Listing right-click action. */
    final String listingActionName;

    /** Action name for the Functions-window action. */
    final String functionActionName;

    /** Popup menu item text (identical in both contexts). */
    final String menuItem;

    /** Help anchor within the Screach help topic. */
    final String helpAnchor;

    /** Short label used when echoing the set address to the results window. */
    final String reportLabel;

    private final BiConsumer<ScreachState, Address> applier;

    ScreachRole(
            String listingActionName,
            String functionActionName,
            String menuItem,
            String helpAnchor,
            String reportLabel,
            BiConsumer<ScreachState, Address> applier) {
        this.listingActionName = listingActionName;
        this.functionActionName = functionActionName;
        this.menuItem = menuItem;
        this.helpAnchor = helpAnchor;
        this.reportLabel = reportLabel;
        this.applier = applier;
    }

    /** Apply an address in this role to {@code state}. */
    void apply(ScreachState state, Address address) {
        applier.accept(state, address);
    }

    /** True if this role accepts more than one address (only {@link #AVOID} does). */
    boolean isMultiValued() {
        return this == AVOID;
    }
}

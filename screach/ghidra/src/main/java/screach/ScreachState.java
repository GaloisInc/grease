package screach;

import ghidra.program.model.address.Address;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Holds the analyst's current Screach inputs: the entry (source) address, the target address, and
 * any addresses to avoid. Listeners (the provider) are notified whenever the state changes so the
 * UI can stay in sync.
 */
class ScreachState {

    /** Notified after any mutation to the state. */
    interface ChangeListener {
        void stateChanged(ScreachState state);
    }

    private Address entry;
    private Address target;
    private final List<Address> avoid = new ArrayList<>();
    private ChangeListener changeListener;

    void setChangeListener(ChangeListener changeListener) {
        this.changeListener = changeListener;
    }

    Address getEntry() {
        return entry;
    }

    void setEntry(Address entry) {
        this.entry = entry;
        fireChanged();
    }

    Address getTarget() {
        return target;
    }

    void setTarget(Address target) {
        this.target = target;
        fireChanged();
    }

    List<Address> getAvoid() {
        return Collections.unmodifiableList(avoid);
    }

    void addAvoid(Address address) {
        if (address != null && !avoid.contains(address)) {
            avoid.add(address);
            fireChanged();
        }
    }

    void clearAvoid() {
        if (!avoid.isEmpty()) {
            avoid.clear();
            fireChanged();
        }
    }

    void clear() {
        entry = null;
        target = null;
        avoid.clear();
        fireChanged();
    }

    /** True when both an entry and a target have been set. */
    boolean isReadyToRun() {
        return entry != null && target != null;
    }

    private void fireChanged() {
        if (changeListener != null) {
            changeListener.stateChanged(this);
        }
    }
}

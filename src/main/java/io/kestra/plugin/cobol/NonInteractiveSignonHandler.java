package io.kestra.plugin.cobol;

import com.ibm.as400.access.SignonEvent;
import com.ibm.as400.access.SignonHandlerAdapter;

/**
 * Prevents interactive GUI prompts for password expiration and sign-on events.
 * Mirrors the pattern used in plugin-jdbc-as400.
 */
final class NonInteractiveSignonHandler extends SignonHandlerAdapter {
    @Override
    public boolean passwordAboutToExpire(SignonEvent event, int daysUntilExpiration) {
        return false;
    }

    @Override
    public boolean passwordExpired(SignonEvent event) {
        return false;
    }

    @Override
    public boolean connectionInitiated(SignonEvent event, boolean forceUpdate) {
        return true;
    }
}

package refactorerl.ui.core.inboxlisteners;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;


public interface RequestHandler {
	OtpErlangObject handle(OtpErlangTuple message);
}

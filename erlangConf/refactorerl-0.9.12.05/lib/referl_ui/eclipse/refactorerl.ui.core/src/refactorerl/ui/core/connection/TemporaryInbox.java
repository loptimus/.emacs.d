package refactorerl.ui.core.connection;

import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;

public class TemporaryInbox {
	private OtpMbox inbox;
	private static Logger logger = Logger.getLogger(TemporaryInbox.class.getName());
	public TemporaryInbox(OtpMbox inbox) {
		this.inbox = inbox;
	}
	
	public OtpErlangTuple receiveMessage() {
		OtpErlangObject result = null;
		try {
			OtpErlangObject msg = inbox.receive(1500);
			if(msg != null) {
				OtpErlangTuple tup = (OtpErlangTuple)msg;
				if(tup.arity() == 3) {
					return receiveMessage();
				}
				if( tup.arity() == 2 &&
				   (tup.elementAt(0) instanceof OtpErlangAtom) &&
				   (((OtpErlangAtom)tup.elementAt(0)).atomValue().equals("rex")) &&
				   (tup.elementAt(1) instanceof OtpErlangAtom) &&
				   (((OtpErlangAtom)tup.elementAt(1)).atomValue().equals("ok"))
				   ) {
					return receiveMessage();
				}
				else {
					result = msg;
				}
			}
		} catch (OtpErlangExit e) {
			e.printStackTrace();
		} catch (OtpErlangDecodeException e) {
			e.printStackTrace();
		}
		logger.info("MessageReceived: " + result);
		return (OtpErlangTuple)result;
	}
}

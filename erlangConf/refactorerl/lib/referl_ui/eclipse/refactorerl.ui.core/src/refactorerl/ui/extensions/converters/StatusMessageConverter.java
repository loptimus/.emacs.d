package refactorerl.ui.extensions.converters;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpErlangList;
import refactorerl.ui.core.extensionpoints.IErlangObjectConverter;

/**
 * Converts a status Erlang object to a String message
 * 
 * @author Gergely Horvath
 * 
 */
public class StatusMessageConverter implements IErlangObjectConverter {
	/*
	 * (non-Javadoc)
	 * @see refactorerl.ui.core.extensionpoints.IErlangObjectConverter#convert(com.ericsson.otp.erlang.OtpErlangTuple)
	 */
	@Override
	public Object convert(OtpErlangTuple tuple) {
		if (tuple.arity() < 3) {
			throw new IllegalArgumentException("Expected type is minimum 3 arity tuple");
		}

		OtpErlangAtom status = (OtpErlangAtom) tuple.elementAt(0);
		if (!status.atomValue().equals("global")) {
			throw new IllegalArgumentException("Expected type of the first element is atom with value 'global'");
		}

		status = (OtpErlangAtom) tuple.elementAt(1);
		if (!status.atomValue().equals("statusinfo")) {
			throw new IllegalArgumentException("Expected type of the second element is atom with value 'statusinfo'");
		}

		if (!(tuple.elementAt(2) instanceof OtpErlangList)) {
			throw new IllegalArgumentException("Third element of the tuple is not an Erlang list");
		}

		return (OtpErlangList) tuple.elementAt(2);	
	}
}

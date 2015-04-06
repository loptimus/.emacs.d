package refactorerl.ui.core.extensionpoints;

import com.ericsson.otp.erlang.OtpErlangTuple;

/**
 * instance of this interface is able to convert an OtpErlangObject into a standard java Object. The Refactorerl Back
 * end always sends OtpErlangObject as a message. it is done by the JInterface API. Converters are to decouple simple
 * Java objects and these OtpErlangObject.
 * 
 * All of the messages sent by Refactorerl are Tuples, which first element is a Atom key. Converters are registered by
 * this key.
 * 
 * @author zoltan verebes
 * 
 */
public interface IErlangObjectConverter {

	/**
	 * Converts a tuple into a java Object
	 * 
	 * @param tuple
	 * @return converted Java object
	 */
	public Object convert(OtpErlangTuple tuple);

}

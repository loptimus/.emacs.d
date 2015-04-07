package refactorerl.ui.core.exceptions;

/**
 * General exception wrapper to wrap any exception during Erlang communication
 * life cycle
 * 
 * @author zoltan verebes
 * 
 */
public class ErlangException extends RuntimeException {
	private static final long serialVersionUID = 7294236675593206963L;

	public ErlangException(String message) {
		super(message);
	}

	public ErlangException(String message, Exception e) {
		super(message, e);
	}
}

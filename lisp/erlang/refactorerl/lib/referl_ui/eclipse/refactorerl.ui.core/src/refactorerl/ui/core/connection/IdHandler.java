package refactorerl.ui.core.connection;
/**
 * @author Gergely Horvath
 *
 */

import java.util.ArrayList;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class IdHandler implements IInboxListener {
	private IConnection connection;
	private ArrayList<OtpErlangTuple> refs;
	
	public IdHandler(IConnection connection) {
		this.connection = connection;
		refs = new ArrayList<OtpErlangTuple>();
	}
	
	public IdHandler() {
		this.connection = ConnectionPool.getInstance().getConnection();
		refs= new ArrayList<OtpErlangTuple>();
	}
	
	@Override
	public void messageReceived(Object message) {
		// TODO Auto-generated method stub
		if (message instanceof OtpErlangTuple) {
			OtpErlangTuple ref = null;
			OtpErlangTuple tuple = (OtpErlangTuple)message;
			
			if ((tuple.arity()>1) && (tuple.elementAt(1) instanceof OtpErlangTuple)) {
				tuple = (OtpErlangTuple) tuple.elementAt(1);
				if (tuple.arity() > 1 && tuple.elementAt(0) instanceof OtpErlangAtom &&
						((OtpErlangAtom)tuple.elementAt(0)).equals(new OtpErlangAtom("reqid"))) {
					ref = tuple;
				}
			}
			
			synchronized (this) {
				if (ref != null) {
					refs.add(ref);
					this.notify();
				}
			}
		}
	}
	
	synchronized public OtpErlangTuple requestId() {
		OtpErlangTuple r = null;
		connection.send("reflib_ui_router", "getid");
		
		try {
			if (refs.isEmpty()) {
				wait();
			}
			r = refs.get(0);
			refs.remove(0);
		}
		catch (InterruptedException e) {}
		
		return r;
	}
}

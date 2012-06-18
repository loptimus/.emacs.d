package refactorerl.ui.core.types;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class RefactorerlFunlist extends OtpErlangList {

	private static final long serialVersionUID = -2986706030512622796L;

	public RefactorerlFunlist(List<String> funs) {
		super(getFuns(funs));
	}

	private static OtpErlangTuple[] getFuns(List<String> funs) {
		List<OtpErlangTuple> tuples = new ArrayList<OtpErlangTuple>();

		for (String fun : funs) {
			String[] parts = fun.split("/");

			if (parts.length != 2) {
				throw new IllegalArgumentException("Illegal fun expression: " + fun);
			}

			tuples.add(new RefactorerlKeyValuePair(parts[0], new OtpErlangInt(Integer.parseInt(parts[1]))));
		}

		return tuples.toArray(new OtpErlangTuple[0]);
	}

}

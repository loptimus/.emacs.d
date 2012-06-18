package refactorerl.ui.core.types;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangList;

public class RefactorerlOrderList extends OtpErlangList {

	private static final long serialVersionUID = -2986706030512622796L;

	public RefactorerlOrderList(List<Integer> order) {
		super(getOrder(order));
	}

	private static OtpErlangInt[] getOrder(List<Integer> order) {
		List<OtpErlangInt> neworder = new ArrayList<OtpErlangInt>();

		for (int i : order) {
			neworder.add(new OtpErlangInt(i));
		}

		return neworder.toArray(new OtpErlangInt[0]);
	}

}

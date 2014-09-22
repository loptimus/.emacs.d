package refactorerl.ui.core.types;

import java.util.ArrayList;
import java.util.List;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangString;

public class RefactorerlReclist extends OtpErlangList {

	private static final long serialVersionUID = -6219682068548266720L;

	public RefactorerlReclist(List<String> records) {
		super(getRecords(records));
	}

	private static OtpErlangString[] getRecords(List<String> recs) {
		List<OtpErlangString> records = new ArrayList<OtpErlangString>();

		for (String record : recs) {
			records.add(new OtpErlangString(record));
		}

		return records.toArray(new OtpErlangString[0]);
	}

}

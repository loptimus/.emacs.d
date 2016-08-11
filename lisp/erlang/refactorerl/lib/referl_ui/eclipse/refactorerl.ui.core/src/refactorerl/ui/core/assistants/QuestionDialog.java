/**
 * 
 */
package refactorerl.ui.core.assistants;

import java.util.logging.Logger;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import refactorerl.ui.core.utils.ProplistUtils;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;

/**
 * @author Gergely Horvath
 * 
 */
public class QuestionDialog extends Dialog {
	private static Logger logger = Logger.getLogger(QuestionDialog.class
			.getName());

	private enum QuestionType {
		info, textbox, yesno, checkbox, radio
	}

	private OtpErlangObject values[];

	public QuestionDialog(Shell parent) {
		super(parent);
	}

	public QuestionDialog(Shell parent, int style) {
		super(parent, style);
	}

	/**
	 * Open the question dialog, create layout, and get answers
	 * 
	 * @param questions
	 *            List of property lists [[{key,value},...]...]
	 * @return List of answers in order the questions, or null if Cancel pressed
	 */
	public OtpErlangList open(OtpErlangList questions) {
		logger.info(questions.toString());

		Shell parent = getParent();
		final Shell shell = new Shell(parent, SWT.TITLE | SWT.BORDER
				| SWT.APPLICATION_MODAL);

		shell.setText("RefactorErl");
		shell.setLayout(new GridLayout(2, false));

		values = new OtpErlangObject[questions.arity()];

		int i = 0;
		GridData gridData = null;

		for (OtpErlangObject o : questions) {
			if (!(o instanceof OtpErlangList)) {
				continue;
			}

			OtpErlangList list = (OtpErlangList) o;

			OtpErlangObject text = ProplistUtils.getValue("text", list);
			OtpErlangObject validator = ProplistUtils.getValue("validator",
					list);
			OtpErlangObject tmp = ProplistUtils.getValue("default", list);
			String defaultValue = (tmp == null) ? null : tmp.toString();

			Label label = new Label(shell, SWT.NULL);
			label.setText(((OtpErlangString) text).stringValue());

			OtpErlangObject type = ProplistUtils.getValue("format", list);

			switch (QuestionType.valueOf(type.toString())) {
			case info:
				gridData = new GridData();
				gridData.horizontalAlignment = SWT.FILL;
				gridData.grabExcessHorizontalSpace = true;
				gridData.verticalAlignment = SWT.FILL;
				gridData.grabExcessVerticalSpace = true;
				gridData.horizontalSpan = 2;
				label.setLayoutData(gridData);
				changeValue("info", i);
				break;

			case textbox:
				addTextWidget(shell, i, defaultValue, validator);
				break;

			case yesno:
				addChechBoxWidget(shell, i, defaultValue, validator);
				break;

			case checkbox:
				addChechBoxWidget(shell, i, defaultValue, validator);
				break;

			case radio:
				addRadioButtonWidget(shell, i, defaultValue, validator);
				break;
			}

			++i;
		}
	    
		Composite composite = new Composite(shell, SWT.NULL);
	    composite.setLayout(new RowLayout());
	    gridData = new GridData();
	    gridData.horizontalAlignment = SWT.CENTER;
	    gridData.horizontalSpan = 2;
	    gridData.grabExcessHorizontalSpace = true;
	    composite.setLayoutData(gridData);
	    
		Button buttonOk = new Button(composite, SWT.PUSH);
		buttonOk.setText("Ok");
		buttonOk.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				shell.dispose();
			}
		});

		Button buttonCancel = new Button(composite, SWT.PUSH);
		buttonCancel.setText("Cancel");
		buttonCancel.addListener(SWT.Selection, new Listener() {
			public void handleEvent(Event event) {
				values = null;
				shell.dispose();
			}
		});

		shell.addListener(SWT.Traverse, new Listener() {
			public void handleEvent(Event event) {
				if (event.detail == SWT.TRAVERSE_ESCAPE) {
					event.doit = false;
				}
			}
		});

		shell.pack();
		shell.open();

		Display display = parent.getDisplay();

		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}

		if (values != null) {
			return new OtpErlangList(values);
		} else {
			return null;
		}
	}

	private Text addTextWidget(Shell parent, final int order,
			String defaultText, OtpErlangObject validator) {
		final Text textBox = new Text(parent, SWT.BORDER);
		final OtpErlangObject valid = validator;

		textBox.setLayoutData(new GridData(120, SWT.DEFAULT));
		
		if (null != defaultText && !defaultText.equals("-1")) {
			textBox.setText(defaultText);
		}

		textBox.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				String data = textBox.getText();

				if (validate(data, "textbox", valid)) {
					changeValue(data, order);
				}
			}
		});

		return textBox;
	}

	private Button addCheckRadioWidget(Shell parent, final int order,
			String defaultText, final int type, OtpErlangObject validator) {
		final Button button = new Button(parent, type);
		final OtpErlangObject valid = validator;

		if (defaultText != null) {
			button.setSelection(defaultText.equals("true")
					|| defaultText.equals("yes"));
		}

		String data = button.getSelection() ? "yes" : "no";

		if (validate(data, (type == SWT.CHECK) ? "checkBox" : "radio", valid)) {
			changeValue(data, order);
		}

		button.addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				String data = button.getSelection() ? "yes" : "no";

				if (validate(data, (type == SWT.CHECK) ? "checkBox" : "radio",
						valid)) {
					changeValueAtom(data, order);
				}
			}
		});

		return button;
	}

	private Button addChechBoxWidget(Shell parent, final int order,
			String defaultText, OtpErlangObject validator) {
		return addCheckRadioWidget(parent, order, defaultText, SWT.CHECK,
				validator);
	}

	private Button addRadioButtonWidget(Shell parent, final int order,
			String defaultText, OtpErlangObject validator) {
		return addCheckRadioWidget(parent, order, defaultText, SWT.RADIO,
				validator);
	}

	private void changeValue(String data, int order) {
		logger.info(data + " " + order);
		if (data != null && !data.equals("")) {
			values[order] = new OtpErlangString(data);
		} else {
			values[order] = new OtpErlangString("");
		}
	}

	private void changeValueAtom(String data, int order) {
		logger.info(data + " " + order);
		if (data != null && !data.equals("")) {
			values[order] = new OtpErlangAtom(data);
		}
	}

	private boolean validate(String data, String type, OtpErlangObject validator) {
		return true;
	}
}

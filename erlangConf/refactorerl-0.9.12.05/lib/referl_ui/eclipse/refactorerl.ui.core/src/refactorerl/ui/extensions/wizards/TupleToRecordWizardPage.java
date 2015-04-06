package refactorerl.ui.extensions.wizards;

import java.util.ArrayList;
import java.util.EmptyStackException;
import java.util.List;
import java.util.Stack;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.handlers.HandlerUtil;

import refactorerl.ui.core.parameterproviders.ManualTransformParameterProvider;

/**
 * 
 * @author Gergely Horvath
 */
public class TupleToRecordWizardPage extends ManualTransformParameterProvider {
	private String nameList[];
	
	public TupleToRecordWizardPage(ExecutionEvent event, String name,
			String description, ArrayList<String> args) {
		super(event, name, description, args);
	}

	@Override
	public void createControl(Composite parent) {
		// TODO Add  record name input, add tuple element name inputs
		final Composite composite = new Composite(parent, SWT.NONE);
		setControl(composite);

		composite.setLayout(new GridLayout(2, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		// 	Create tuple elements list
		List<String> parameters;
		try  {
			parameters = getTupleElements();
			if (parameters == null) {
				setErrorMessage("Selection is not a valid tuple!");
				setPageComplete(false);
				return;
			}
			
			nameList = new String[parameters.size()+1];
			createLabelTextInput(composite, 0, "Record name");
			
			int i =1;
			for (String element : parameters) {
				createLabelTextInput(composite, i, element);
				i++;
			}
		} catch (ExecutionException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		validate();
	}

	private List<String> getTupleElements() throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelectionChecked(event);

		if (!(selection instanceof ITextSelection)) {
			throw new ExecutionException("Selection is not a text selection of Erlang Editor");
		}

		ITextSelection textSelection = (ITextSelection) selection;
		String head = textSelection.getText();
		if (head.startsWith("{") && head.endsWith("}")) {
			head = head.substring(1, head.length() - 1);
		} else  {
			return null;
		}
		
		try {
			List<String> result = new ArrayList<String>();
			Stack<Character> stack = new Stack<Character>();

			StringBuffer buffer = new StringBuffer();
			for (char ch : head.toCharArray()) {
				if (ch == ',' && stack.empty()) {
					result.add(buffer.toString().trim());
					buffer = new StringBuffer();
				} else {
					buffer.append(ch);

					if (ch == '{' || ch == '[') {
						stack.push(ch);
					} else if (ch == '}' || ch == ']') {
						stack.pop();
					}
				}
			}
		
			if (!buffer.toString().trim().equals("")) {
				result.add(buffer.toString().trim());
			}
			
			return result;
		} catch (EmptyStackException e) {
			return null;
		}
	}
	
	private Text createLabelTextInput(final Composite composite, final int i, final String labelText) {
		Label label = new Label(composite, SWT.NONE);
		label.setText(labelText + ": ");
		label.setLayoutData(new GridData());

		final Text field = new Text(composite, SWT.BORDER);
		field.setText("");
		field.setFont(composite.getFont());
		field.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));

		field.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				addNameInput(field.getText(), i);
				validate();
				addParameters();
			}
		});

		return field;
	}
	
	private void addNameInput(String name, int i) {
		nameList[i] = name;
	}
	
	private void addParameters() {
		addParameter("name", nameList[0]);
		String s = "";
		for (int i = 1; i < nameList.length - 1; ++i) {
			s += nameList[i] + " "; 
		}
		s += nameList[nameList.length - 1];
		addParameter("text", s);
	}
	
	private void validate() {
		if (null == nameList[0] || nameList[0].equals("")) {
			setErrorMessage("Record name is required");
			setPageComplete(false);
			return;
		}
		
		for (int i = 1; i < nameList.length; i++) {
			if (null == nameList[i] || nameList[i].equals("")) {
				setErrorMessage("All of record field name is required");
				setPageComplete(false);
				return;
			}
		}
		setErrorMessage(null);
		setPageComplete(true);
	}
}

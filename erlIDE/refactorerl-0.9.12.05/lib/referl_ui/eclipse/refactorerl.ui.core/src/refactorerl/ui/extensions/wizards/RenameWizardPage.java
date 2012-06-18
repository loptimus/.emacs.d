package refactorerl.ui.extensions.wizards;

import java.util.ArrayList;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import refactorerl.ui.core.parameterproviders.ManualTransformParameterProvider;

/**
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public class RenameWizardPage extends ManualTransformParameterProvider {
	private Text nameText;
	private Text variableText;
	
	public RenameWizardPage(ExecutionEvent event, String name, String description, ArrayList<String> args) {
		super(event, name, description, args);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createControl(Composite parent) {
		final Composite composite = new Composite(parent, SWT.NONE);
		setControl(composite);

		composite.setLayout(new GridLayout(2, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		composite.setFont(parent.getFont());

		if (args.contains(NAME.toUpperCase())) {
			nameText = createLabelTextInput(composite, NAME, "New name");
		}
		
		if (args.contains(FILENAME.toUpperCase())) {
			nameText = createLabelTextInput(composite, FILENAME, "New name");
		}

		if (args.contains(MACNAME.toUpperCase())) {
			nameText = createLabelTextInput(composite, MACNAME, "New macro name");
		}

		if (args.contains(VARNAME.toUpperCase())) {
			variableText = createLabelTextInput(composite, VARNAME, "New variable name");
		}
		
		if (args.contains(QUERYSTR.toUpperCase())) {
			variableText = createLabelTextInput(composite, QUERYSTR, "Semantic query");
		}

		validate();
	}

	private Text createLabelTextInput(final Composite composite, final String key, final String labelText) {
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
				validate();
				if (!key.equals(FILENAME))
					addParameter(key, field.getText());
				else
					addParameter(key, field.getText() + ".hrl");
			}
		});

		return field;
	}

	private void validate() {
		//TODO: check if the name is not used in the target module: message need to be sent to the Erlang process to query it

		if ((args.contains(NAME.toUpperCase()) || args.contains(FILENAME.toUpperCase()) || args.contains(MACNAME.toUpperCase())) && (nameText.getText() == null || nameText.getText().equals(""))) {
			setErrorMessage("New name is required");
			setPageComplete(false);
			return;
		}

		if (args.contains(VARNAME.toUpperCase())
				&& (variableText.getText() == null || variableText.getText().equals("") || (variableText.getText().charAt(0)<'A' || variableText.getText().charAt(0)>'Z'))) {
			setErrorMessage("New variable name is required");
			setPageComplete(false);
			return;
		}

		setErrorMessage(null);
		setPageComplete(true);
	}
}

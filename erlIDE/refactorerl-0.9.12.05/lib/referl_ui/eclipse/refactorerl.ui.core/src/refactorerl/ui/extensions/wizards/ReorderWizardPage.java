package refactorerl.ui.extensions.wizards;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.erlang.IErlFunctionClause;
import org.erlide.ui.editors.erl.ErlangEditor;

import refactorerl.ui.Images;
import refactorerl.ui.core.parameterproviders.ManualTransformParameterProvider;
import refactorerl.ui.core.utils.SelectionUtils;

/**
 * Wizard page to display a list and let the user to reorder its elements. It creates
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */

@SuppressWarnings("restriction")
public class ReorderWizardPage extends ManualTransformParameterProvider {
	private ListViewer viewer;

	private List<Integer> order;

	public ReorderWizardPage(ExecutionEvent event, String name, String description, ArrayList<String> args) {
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

		// list
		viewer = new ListViewer(composite, SWT.BORDER);
		viewer.setContentProvider(new ArrayContentProvider());
		viewer.setLabelProvider(new LabelProvider() {
			@Override
			public String getText(Object element) {
				return super.getText(element);
			}

			@Override
			public Image getImage(Object element) {
				if (element instanceof IFile) {
					return Images.ERLANG_IMG;
				}

				return super.getImage(element);
			}

		});
		viewer.getList().setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final Composite buttonPanel = new Composite(composite, SWT.NONE);
		buttonPanel.setLayout(new GridLayout(1, true));
		final Button upButton = new Button(buttonPanel, SWT.BORDER);
		upButton.setText("Up");
		upButton.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

		final Button downButton = new Button(buttonPanel, SWT.BORDER);
		downButton.setText("Down");
		downButton.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

		upButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				moveSelection(-1);
			}
		});

		downButton.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent event) {
				moveSelection(1);
			}
		});

		List<String> parameters = getFunctionParameters();
		viewer.setInput(parameters);
		if (parameters != null) {
			order = new ArrayList<Integer>();
			for (int i = 0; i < parameters.size(); i++) {
				order.add(i + 1);
			}
		}
	}

	@SuppressWarnings("unchecked")
	private void moveSelection(int i) {
		IStructuredSelection selection = (IStructuredSelection)viewer.getSelection();

		if (selection.isEmpty()) {
			return;
		}

		List<Object> input = (List<Object>) viewer.getInput();
		int index = input.indexOf(selection.getFirstElement());

		if (index + i < 0 || index + i >= input.size()) {
			return;
		}

		int other = order.get(index);
		order.set(index, order.get(index + i));
		order.set(index + i, other);
		addParameter(ORDER, order);

		input.set(index, input.get(index + i));
		input.set(index + i, selection.getFirstElement());
		viewer.setInput(input);

		viewer.setSelection(selection, true);
	}

	private List<String> getFunctionParameters() {
		try {
			ISelection currentSelection = HandlerUtil.getCurrentSelectionChecked(event);

			if (currentSelection instanceof ITextSelection) {
				ErlangEditor editor = SelectionUtils.getActiveErlangEditor();
				IErlElement element = SelectionUtils.getCurrentSelectedEditorElement(editor, (ITextSelection) currentSelection);
				if (element instanceof IErlFunctionClause) {
					IErlFunctionClause func = (IErlFunctionClause) element;
					String head = func.getHead();
					// remove parentheses
					head = head.substring(1, head.length() - 1);
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
				}

			}
		} catch (ExecutionException e) {
			return null;
		}
		
		return null;
	}
}

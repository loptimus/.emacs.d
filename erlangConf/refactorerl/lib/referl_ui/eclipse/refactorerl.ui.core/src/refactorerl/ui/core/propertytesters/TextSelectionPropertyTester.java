package refactorerl.ui.core.propertytesters;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.ITextSelection;
import org.erlide.core.model.erlang.IErlAttribute;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlMember;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.ui.editors.erl.ErlangEditor;

import refactorerl.ui.core.utils.SelectionUtils;

/**
 * ErlangEditor is responsible for displaying Erlang modules. It provides an ITextSelection through its TextEditor superclass to let the
 * contributor know the current selected area in the editor.
 * 
 * Refactrerl refactors can be applied only on some special elements of the module. This class is called by Eclipse Common Command API to check
 * whether a functionality (in this case the Refactors) is available for the current selection, at the current cursor position in the editor.
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */

@SuppressWarnings("restriction")
public class TextSelectionPropertyTester extends PropertyTester {

	private static final String FUNCTION = "function";
	private static final String MODULE = "module";
	private static final String VARIABLE = "variable";
	private static final String RECORD = "record";
	private static final String RECORD_FIELD = "recordfield";
	
	@Override
	public boolean test(Object receiver, String property, Object[] args, Object expectedValue) {

		if (!(receiver instanceof ITextSelection)) {
			return false;
		}

		ITextSelection selection = (ITextSelection) receiver;

		ErlangEditor editor = SelectionUtils.getActiveErlangEditor();
		IErlMember member = (IErlMember) SelectionUtils.getCurrentSelectedEditorElement(editor, selection);
	
		// is function member of the module?
		if (FUNCTION.equals(property) && member instanceof IErlFunction) {
			return true;
//			ISourceRange range = member.getNameRange();
//			return selection.getOffset() < range.getOffset() + range.getLength();
		}

		// is module definition of the module?
		if (MODULE.equals(property) && member instanceof IErlAttribute) {
			IErlAttribute attribute = (IErlAttribute) member;
			return attribute.getName().equals(MODULE);
		}

		// is variable in the ErlFunction?
		if (VARIABLE.equals(property) && member instanceof IErlFunction) {
			String variable = null;
			try {
				variable = SelectionUtils.getVariable(editor, selection);
			} catch (BadLocationException e) {
				throw new IllegalArgumentException("Exception occured when variable was checked", e);
			}
			return variable != null;
		}

		if (RECORD.equals(property) && member instanceof IErlRecordDef) {
			//IErlRecordDef record = (IErlRecordDef) member;
			return true;
//			return record.getDefinedName() != null && record.getDefinedName().equals(selection.getText()); 
		}

		if (RECORD_FIELD.equals(property) && member instanceof IErlRecordDef) {
			//IErlRecordDef record = (IErlRecordDef) member;
			//	return selection.getText() != null && record.getFields().contains(selection.getText());
			return true;
		}

		
		return false;
	}

}

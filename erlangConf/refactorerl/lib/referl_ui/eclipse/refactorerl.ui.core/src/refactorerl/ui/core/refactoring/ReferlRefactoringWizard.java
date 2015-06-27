package refactorerl.ui.core.refactoring;

import java.util.List;
import java.util.Map;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.parameterproviders.ITransformParameterProvider;
import refactorerl.ui.core.parameterproviders.ManualTransformParameterProvider;

/**
 * @author Zolt�n Verebes, modified by B�la - Tam�s J�zsa
 * It was a nested class of ReferlCommandHandler.
 * It has the role to handle refactoring process.
 */
public class ReferlRefactoringWizard extends RefactoringWizard {
	private final List<ITransformParameterProvider> parameterProvidersPages;
	private final Map<String, Object> parameters;
	
	public ReferlRefactoringWizard(
			Refactoring refactoring,
			int flags,
			List<ITransformParameterProvider>parameterProviders,
			Map<String, Object> pParameters) {
		
		super(refactoring, flags);
		parameterProvidersPages = parameterProviders;
		parameters = pParameters;
	}

	@Override
	protected void addUserInputPages() {
		for (ITransformParameterProvider transformParameterProvider : parameterProvidersPages) {
			ManualTransformParameterProvider page = (ManualTransformParameterProvider)transformParameterProvider;
			page.setParameterMap(parameters);
			addPage(page);
		}
	}
	
	@Override
	public boolean performCancel() {
		if (getRefactoring() instanceof ReferlRefactoring) {
			if (((ReferlRefactoring)getRefactoring()).getUndoable()) {
				ConnectionPool.getInstance().getConnection().getReferlUI().undo();
			}
		}
		
		return true;
	}
}

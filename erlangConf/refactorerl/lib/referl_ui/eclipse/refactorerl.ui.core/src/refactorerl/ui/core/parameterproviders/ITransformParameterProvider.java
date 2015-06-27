package refactorerl.ui.core.parameterproviders;

import java.util.Map;

import org.eclipse.core.commands.ExecutionException;

import refactorerl.ui.core.constants.ReferlArgsConstants;
import refactorerl.ui.core.constants.ReferlConstants;



/**
 * Common interface for ContextTransformParameterProvider and ManualInputParameterProvider classes. Subclass of
 * ContextTransformParameter is to collect transform parameters from the Command handler context. It happens
 * automatically when the the user starts the refactors.
 * 
 * ManualInputParameter is not only a TransformParameterprovider, it extends the UserInputWizardPage that is used in
 * RefactoringWizard.
 * 
 * @author zoltan verebes
 * 
 */
public interface ITransformParameterProvider extends ReferlArgsConstants, ReferlConstants {

	/**
	 * Collected parameters by the this object. Object is converted to one of the subclasses of OtpErlangObject. The map
	 * key is used to decide which of the many sublcasses is used
	 * 
	 * @return map of string and object pairs
	 */
	public Map<String, Object> getParameterMap() throws ExecutionException;

}

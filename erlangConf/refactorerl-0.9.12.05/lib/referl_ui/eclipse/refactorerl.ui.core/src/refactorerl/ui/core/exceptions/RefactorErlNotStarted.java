package refactorerl.ui.core.exceptions;

public class RefactorErlNotStarted extends Exception {
	private static final long serialVersionUID = 2452177608766640844L;

	public RefactorErlNotStarted() {
		super("RefactorErl process not present");
	}
}

package refactorerl.ui.core.inboxlisteners;


public abstract class RequestHandlerAdapter implements RequestHandler{

	protected RequestListener requestListener;
	protected void removeRequestListener() {
		requestListener.removeListener();
	}
}

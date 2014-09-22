package refactorerl.ui;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

/**
 * Creates and keeps reference to frequently used images
 * 
 * @author zoltan verebs
 * 
 */
public class Images {

	public static final Image ERLANG_IMG = ImageDescriptor.createFromFile(Activator.class, "icon/erlang.png").createImage();

}

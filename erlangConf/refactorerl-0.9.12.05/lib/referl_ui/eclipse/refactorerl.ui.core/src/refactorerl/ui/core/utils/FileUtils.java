package refactorerl.ui.core.utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
/**
* 
* @author  Gergely Horvath 
* 
*/
public class FileUtils {
	public static void delete(String fileName) {
		File file = new File(fileName);
		
		if (!file.exists()) {
			throw new IllegalArgumentException("Delete: no such file or directory: " + fileName);
		}
		
		if (!file.canWrite()) {
			throw new IllegalArgumentException("Delete: write protected: " + fileName);
		}
		
		if (file.isDirectory()) {
			String[] files = file.list();
			if (files.length > 0) {
				throw new IllegalArgumentException("Delete: directory not empty: " + fileName);
			}
		}
		
		boolean success = file.delete();
		if (!success) {
			throw new IllegalArgumentException("Delete: deletion failed");
		}
	}
	
	public static void copy(String fromFileName, String toFileName) throws IOException {
		File fromFile = new File(fromFileName);
		File toFile = new File(toFileName);
		
		if (!fromFile.exists()) {
			throw new IOException("FileCopy: " + "no such source file: " + fromFileName);
		} else if (!fromFile.isFile()) {
			throw new IOException("FileCopy: " + "can't copy directory: " + fromFileName);
		} else if (!fromFile.canRead()) {
			throw new IOException("FileCopy: " + "source file is unreadable: " + fromFileName);
		}
		
		if (toFile.isDirectory()) {
			toFile = new File(toFile, fromFile.getName());
		}
		
		if (toFile.exists()) {
			if (!toFile.canWrite()) {
				throw new IOException("FileCopy: " + "destination file is unwriteable: " + toFileName);
			}
			
			String parent = toFile.getParent();
			if (null == parent) {
				parent = System.getProperty("user.dir");
			}
			
			File dir = new File(parent);
			if (!dir.exists()) {
				throw new IOException("FileCopy: destination directory doesn't exist: " + parent);
			} else if (dir.isFile()) {
				throw new IOException("FileCopy: destination is not a directory: " + parent);
			} else if (!dir.canWrite()) {
				throw new IOException("FileCopy: destination directory is unwriteable: " + parent);
			}
		}
		
		FileInputStream from = null;
		FileOutputStream to = null;
		try {
			from = new FileInputStream(fromFile);
			to = new FileOutputStream(toFile);
			byte[] buffer = new byte[4096];
			int bytesRead;
			
			while ((bytesRead = from.read(buffer)) != -1) {
				to.write(buffer, 0, bytesRead); 
			}
		} finally {
			if (null != from) {
				try {
					from.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			
			if (null != to) {
				try {
					to.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}
}

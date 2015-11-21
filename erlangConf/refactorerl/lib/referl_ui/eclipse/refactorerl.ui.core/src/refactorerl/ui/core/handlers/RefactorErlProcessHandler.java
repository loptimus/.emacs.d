package refactorerl.ui.core.handlers;

import refactorerl.ui.core.exceptions.ErlangException;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class RefactorErlProcessHandler extends Thread {
	private Process refactorerlRuntime;
	
	public RefactorErlProcessHandler(String name, Process refactorerlRuntime) {
		this.refactorerlRuntime = refactorerlRuntime;
		this.setName(name);
	}
	
	public void run() {
		String line = null;
		
		try {
			BufferedReader stdIn	= new BufferedReader(new InputStreamReader(refactorerlRuntime.getInputStream()));
			BufferedReader stdError = new BufferedReader(new InputStreamReader(refactorerlRuntime.getErrorStream()));
			
			int result = refactorerlRuntime.waitFor();
			
            try {
            	while ((line = stdIn.readLine()) != null) {
				    System.out.println(line);
				}
            	
				while ((line = stdError.readLine()) != null) {
				    System.err.println(line);
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
			
			System.err.println("Refactorerl Process exited: " + result);
		}
		catch (InterruptedException e) {
			throw new ErlangException("Could not start Refactorerl process", e);
		}
	}
}

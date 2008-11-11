import ij.plugin.PlugIn;
import ij.WindowManager;
import ij.gui.*;
import mroi.*;
/* To use commands, execute a macro:
 * call("MroiCommand.run","load");
 * where you change "load" to whatever you want.
 */
public class MroiCommand {

	public static void run(String lbl) {
		ImageCanvas c = WindowManager.getCurrentWindow().getCanvas();
		if (c instanceof MroiCanvas) {
			try {
				((MroiCanvas)c).command(lbl);
			} catch (NoSuchCommandException e) {
				new MessageDialog(WindowManager.getCurrentWindow(), "No such command", "No such command: " + e.getMessage());
			}
		} else {
			new MessageDialog(WindowManager.getCurrentWindow(), "Not an Mroi window", "Mroi commands must be run on a window where mroi is active.");
		}

	}

}

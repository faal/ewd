import java.util.List;

import com.ericsson.otp.erlang.*;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.RenderedWebElement;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;

public class WD {
    public enum InstanceType {
	FIREFOX, CHROME
	    }

    public static void main(String[] args)  {
	Server server = new Server();
	try {
	    server.start();
	}
	catch (java.io.IOException e) {
	    System.out.println("Error while starting node");
	}
	catch(com.ericsson.otp.erlang.OtpErlangExit e) {
	    System.out.println("Error while starting node2");
	}
	catch(com.ericsson.otp.erlang.OtpErlangDecodeException e) {
	        System.out.println("Error while starting node3");
	}
    }
}

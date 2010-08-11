import com.ericsson.otp.erlang.*;

import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.WebDriver;

public class Instance extends Thread {
    OtpMbox mbox;
    WebDriver driver;

    Instance(OtpNode node) {
	mbox = node.createMbox();
	driver = new FirefoxDriver();
	start();
    }

    public void run() {
	while (true) {
	    try {
		System.out.println("Instance waiting");
		OtpErlangObject o = mbox.receive();
		System.out.println("received");
		OtpErlangTuple msg;
		OtpErlangPid from;
		msg = (OtpErlangTuple)o;
		from = (OtpErlangPid)(msg.elementAt(0));
		OtpErlangAtom fun = (OtpErlangAtom)(msg.elementAt(1));
		OtpErlangObject arg = (OtpErlangObject)(msg.elementAt(2));
		execute(mbox, from, fun, arg);
	    }
	    catch (OtpErlangExit e) {
		e.printStackTrace();
	    }
	    catch (OtpErlangDecodeException e) {
		e.printStackTrace();
	    }
	}
    }

    public OtpErlangPid get_pid() {

	return mbox.self();

    }
    public void execute(OtpMbox mbox, OtpErlangPid from,
			OtpErlangAtom fun0, OtpErlangObject Arg) {
	OtpErlangObject[] ret = new OtpErlangObject[] {
	    mbox.self(),
	    fun0,
	    null
	};
	String fun = fun0.toString();
	if (fun.compareTo("get") == 0) {
	    String url = ((OtpErlangString)Arg).stringValue();
	    System.out.println(url);
	    driver.get(url);
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("get_current_url") == 0) {
	    ret[2] = new OtpErlangString(driver.getCurrentUrl());
	} else if (fun.compareTo("get_title") == 0) {
	    ret[2] = new OtpErlangString(driver.getTitle());
	}
	mbox.send(from, new OtpErlangTuple(ret));


    }
}

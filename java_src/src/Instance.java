import com.ericsson.otp.erlang.*;

import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.WebDriver;


import java.util.*;

public class Instance extends Thread {
    OtpMbox mbox;
    WebDriver driver;

    Instance(OtpNode node, WD.InstanceType type) {
	mbox = node.createMbox();
	switch (type) {
	case FIREFOX: driver = new FirefoxDriver(); break;
	case CHROME: driver = new ChromeDriver(); break;
	}
	
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
	} else if (fun.compareTo("close") == 0) {
	    driver.close();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("quit") == 0) {
	    driver.quit();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("get_page_src") == 0) {
	    ret[2] = new OtpErlangString(driver.getPageSource());
	} else if (fun.compareTo("back") == 0) {
	    driver.navigate().back();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("forward") == 0) {
	    driver.navigate().forward();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("refresh") == 0) {
	    driver.navigate().refresh();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("get_window") == 0) {
	    ret[2] = new OtpErlangString(driver.getWindowHandle());
	} else if (fun.compareTo("get_windows") == 0) {
	    Iterator<String> iter = driver.getWindowHandles().iterator();
	    int Size = driver.getWindowHandles().size();
	    OtpErlangString [] s = new OtpErlangString [Size];
	    for (int i = 0; i < Size; i++) {
		s[i] = new OtpErlangString(iter.next());
	    }
	    ret[2] = new OtpErlangList(s);
	} else if (fun.compareTo("target_window") == 0) {
	    String window = ((OtpErlangString)Arg).stringValue();
	    driver.switchTo().window(window);
	    ret[2] = new OtpErlangAtom("ok");
	}


	mbox.send(from, new OtpErlangTuple(ret));


    }
}

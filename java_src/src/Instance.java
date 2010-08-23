import com.ericsson.otp.erlang.*;

import org.openqa.selenium.firefox.FirefoxDriver;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.By;
import org.openqa.selenium.WebElement;

import java.util.*;

public class Instance extends Thread {
    OtpMbox mbox;
    WebDriver driver;
    Hashtable elements = new Hashtable();

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
		OtpErlangObject o = mbox.receive();
		OtpErlangTuple msg;
		OtpErlangPid from;
		msg = (OtpErlangTuple)o;
		from = (OtpErlangPid)(msg.elementAt(0));
		OtpErlangAtom fun = (OtpErlangAtom)(msg.elementAt(1));
		OtpErlangObject arg = (OtpErlangObject)(msg.elementAt(2));
		System.out.println("pre decay");
		decay();
		System.out.println("post decay");
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

    private void decay() {
	Enumeration keys = elements.keys();
	while(keys.hasMoreElements()) {
	    Object key = keys.nextElement();
	    Object[] elem = (Object[])elements.get(key);
	    elem[1] = new Integer(((Integer)elem[1]).intValue() -1);
	    if (((Integer)elem[1]).intValue() == 0) {
		elements.remove(key);
	    }
	    System.out.println(elem[0]);
	    System.out.println(elem[1].toString());	    

	}
    }
    
    private UUID insert_elem(WebElement elem) {
	UUID uuid = UUID.randomUUID();
	elements.put(uuid.toString(), new Object[] {elem,
						    new Integer(3)});
	return uuid;
	
    }
    
    private WebElement get_elem(String S) {
	Object[] val = (Object [])elements.get(S);
	return (WebElement)val[0];
    }

    public void execute(OtpMbox mbox, OtpErlangPid from,
			OtpErlangAtom fun0, OtpErlangObject Arg) {
	OtpErlangObject[] ret = new OtpErlangObject[] {
	    mbox.self(),
	    fun0,
	    null
	};
	String fun = fun0.toString();
	System.out.println("running" + fun);
	if (fun.compareTo("get") == 0) {
	    String url = ((OtpErlangString)Arg).stringValue();
	    driver.get(url);
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("current_url") == 0) {
	    ret[2] = new OtpErlangString(driver.getCurrentUrl());
	} else if (fun.compareTo("title") == 0) {
	    ret[2] = new OtpErlangString(driver.getTitle());
	} else if (fun.compareTo("close") == 0) {
	    driver.close();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("quit") == 0) {
	    driver.quit();
	    ret[2] = new OtpErlangAtom("ok");
	} else if (fun.compareTo("page_src") == 0) {
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
	} else if (fun.compareTo("window") == 0) {
	    ret[2] = new OtpErlangString(driver.getWindowHandle());
	} else if (fun.compareTo("windows") == 0) {
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

	/* Element interaction functions*/
	else if (fun.compareTo("elem_by_id") == 0) {
	    String id = ((OtpErlangString)Arg).stringValue();
	    try {
		WebElement elem = driver.findElement(By.id(id));
		UUID uuid = insert_elem(elem);
		ret[2] = new OtpErlangTuple(
					    new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						new OtpErlangAtom(uuid.toString())
					    });
	    }

	    catch (org.openqa.selenium.NoSuchElementException e) {
		ret[2] = new OtpErlangTuple(
					    new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangAtom("no_elem")
					    });
		    }
	} else if (fun.compareTo("elems_by_id") == 0) {
	    String id = ((OtpErlangString)Arg).stringValue();
	    try {
		List<WebElement> elems = driver.findElements(By.id(id));
		Iterator itr = elems.iterator();
		List<OtpErlangAtom> result = new LinkedList<OtpErlangAtom> ();
		while (itr.hasNext()) {
		    UUID uuid = insert_elem((WebElement)itr.next());
		    result.add(new OtpErlangAtom(uuid.toString()));
		    
		}
		
		ret[2] = new OtpErlangTuple(
					    new OtpErlangObject[] {
						new OtpErlangAtom("ok"),
						new OtpErlangList(result.toArray(new OtpErlangAtom[0]))
					    });
	    }

	    catch (org.openqa.selenium.NoSuchElementException e) {
		ret[2] = new OtpErlangTuple(
					    new OtpErlangObject[] {
						new OtpErlangAtom("error"),
						new OtpErlangAtom("no_elem")
					    });
		    }
	} else if (fun.compareTo("text") == 0) {
	    String ID = ((OtpErlangAtom)Arg).atomValue();
	    ret[2] = new OtpErlangString(get_elem(ID).getText());
	}

	mbox.send(from, new OtpErlangTuple(ret));


    }
}

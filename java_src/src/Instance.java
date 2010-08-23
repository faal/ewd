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
		OtpErlangTuple msg = (OtpErlangTuple)(mbox.receive());
		OtpErlangPid from = (OtpErlangPid)(msg.elementAt(0));
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

    private WD.Fun parse_fun(String S) {
	if (new String("get").equals(S))
	    return WD.Fun.GET;
	else if (new String("current_url").equals(S))
	    return WD.Fun.CURRENT_URL;
	else if (new String("title").equals(S))
	    return WD.Fun.TITLE;
	else if (new String("close").equals(S))
	    return WD.Fun.CLOSE;
	else if (new String("quit").equals(S))
	    return WD.Fun.QUIT;
	else if (new String("page_src").equals(S))
	    return WD.Fun.PAGE_SRC;
	else if (new String("back").equals(S))
	    return WD.Fun.BACK;
	else if (new String("forward").equals(S))
	    return WD.Fun.FORWARD;
	else if (new String("refresh").equals(S))
	    return WD.Fun.REFRESH;
	else if (new String("window").equals(S))
	    return WD.Fun.WINDOW;
	else if (new String("windows").equals(S))
	    return WD.Fun.WINDOWS;
	else if (new String("target_window").equals(S))
	    return WD.Fun.TARGET_WINDOW;
	else if (new String("elem_by_id").equals(S))
	    return WD.Fun.ELEM_BY_ID;
	else if (new String("elems_by_id").equals(S))
	    return WD.Fun.ELEMS_BY_ID;
	else if (new String("text").equals(S))
	    return WD.Fun.TEXT;
	return null;
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
	WD.Fun fun2 = parse_fun(fun0.atomValue());

	OtpErlangObject[] ret = new OtpErlangObject[] {
	    mbox.self(),
	    fun0,
	    null
	};

	String fun = fun0.toString();
	System.out.println("running" + fun);
	switch (fun2) {
	case GET:
	    String url = ((OtpErlangString)Arg).stringValue();
	    driver.get(url);
	    ret[2] = new OtpErlangAtom("ok");
	    break;

	case TITLE:
	    ret[2] = new OtpErlangString(driver.getTitle());
	    break;
	    
	case CURRENT_URL:
	    ret[2] = new OtpErlangString(driver.getCurrentUrl());
	    break;

	case CLOSE:
	    driver.close();
	    ret[2] = new OtpErlangAtom("ok");
	    break;
	    
	case QUIT:
	    driver.quit();
	    ret[2] = new OtpErlangAtom("ok");
	    break;

	case PAGE_SRC:
	    ret[2] = new OtpErlangString(driver.getPageSource());
	    break;
	    
	case BACK:
	    driver.navigate().back();
	    ret[2] = new OtpErlangAtom("ok");
	    break;
	    
	case FORWARD:
	    driver.navigate().forward();
	    ret[2] = new OtpErlangAtom("ok");
	    break;

	case REFRESH:
	    driver.navigate().refresh();
	    ret[2] = new OtpErlangAtom("ok");
	    break;

	case WINDOW:
	    ret[2] = new OtpErlangString(driver.getWindowHandle());
	    break;
	
	case WINDOWS:
	    Iterator<String> iter = driver.getWindowHandles().iterator();
	    int Size = driver.getWindowHandles().size();
	    OtpErlangString [] s = new OtpErlangString [Size];
	    for (int i = 0; i < Size; i++) {
		s[i] = new OtpErlangString(iter.next());
	    }
	    ret[2] = new OtpErlangList(s);
	    break;
	    
	case TARGET_WINDOW:
	    String window = ((OtpErlangString)Arg).stringValue();
	    driver.switchTo().window(window);
	    ret[2] = new OtpErlangAtom("ok");
	    break;

	    /* ELEMENT INTERACTION FUNCTIONS*/
	case ELEM_BY_ID:
	    String elem_id1 = ((OtpErlangString)Arg).stringValue();
	    try {
		WebElement elem = driver.findElement(By.id(elem_id1));
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
	    break;

	case ELEMS_BY_ID:
	    String elem_id2 = ((OtpErlangString)Arg).stringValue();
	    try {
		List<WebElement> elems = driver.findElements(By.id(elem_id2));
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
	    break;
	    
	case TEXT:
	    String ID = ((OtpErlangAtom)Arg).atomValue();
	    ret[2] = new OtpErlangString(get_elem(ID).getText());
	    
	}

	mbox.send(from, new OtpErlangTuple(ret));


    }
}

import java.io.*;

import com.ericsson.otp.erlang.*;

import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.RenderedWebElement;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.firefox.FirefoxDriver;


public class Server {
    private OtpNode node;

    private BufferedWriter out;

    private static String COOKIE = "Yjb5XSNf";

    private static String MBOX = "server";
    public void start() throws java.io.IOException,
			       com.ericsson.otp.erlang.OtpErlangExit,
			       com.ericsson.otp.erlang.OtpErlangDecodeException {
	FileWriter fstream = new FileWriter("out.txt");
        out = new BufferedWriter(fstream);
	node = new OtpNode("WD@localhost", COOKIE);
	OtpMbox mbox = node.createMbox(MBOX);
	System.out.println("Starting\n");

	OtpErlangObject o;
	OtpErlangTuple msg;
	OtpErlangPid from;
	while (true) {
	    System.out.println("wait");
	    o = mbox.receive();
	    System.out.println("received");
	    if (o instanceof OtpErlangTuple) {
		msg = (OtpErlangTuple)o;
		from = (OtpErlangPid)(msg.elementAt(0));
		OtpErlangAtom fun = (OtpErlangAtom)(msg.elementAt(1));
		OtpErlangObject arg = (OtpErlangObject)(msg.elementAt(2));
		execute(node, mbox, from, fun, arg);
	    }
	}

    }

    public void execute(OtpNode node, OtpMbox mbox,
			OtpErlangPid from, OtpErlangAtom fun0, 
			OtpErlangObject arg) throws java.io.IOException {
	String fun = fun0.toString();
	System.out.println(fun);

	if (fun.compareTo("sync") == 0) {
	    mbox.send(from, new OtpErlangTuple (
						new OtpErlangObject[] {
						    new OtpErlangAtom(MBOX),
						    fun0,
						    new OtpErlangAtom("ok")}
						));
	} else if (fun.compareTo("new") == 0) {
	    WD.InstanceType type = WD.InstanceType.FIREFOX;
	    System.out.println(((OtpErlangAtom)arg).atomValue());
	    if(((OtpErlangAtom)arg).atomValue().compareTo("firefox") == 0)
		type = WD.InstanceType.FIREFOX;
	    if(((OtpErlangAtom)arg).atomValue().compareTo("chrome") == 0)
		type = WD.InstanceType.CHROME;
	    Instance instance = new Instance(node, type);
	    

	    mbox.send(from, new OtpErlangTuple (
						new OtpErlangObject[] {
						    new OtpErlangAtom(MBOX),
						    fun0,
						    instance.get_pid()}
						));


	} else if (fun.compareTo("stop") == 0) {
	    System.exit(0);
	}

    }

}
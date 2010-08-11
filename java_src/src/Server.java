import java.io.*;

import java.util.UUID;

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
		execute(node, mbox, from, fun);
	    }
	}

    }

    public void execute(OtpNode node, OtpMbox mbox,
			OtpErlangPid from, OtpErlangAtom fun0) throws java.io.IOException {
	String fun = fun0.toString();
	System.out.println(fun);
	if (fun.compareTo("new") == 0) {
	    Instance instance = new Instance(node);

	    UUID id = UUID.randomUUID();
	    System.out.println(id);
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
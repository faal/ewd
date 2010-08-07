import com.ericsson.otp.erlang.*;

public class Server {
    private OtpNode node;

    public void start() throws java.io.IOException {
	node = new OtpNode("WD@localhost", "Yjb5XSNf");
	OtpMbox mbox = node.createMbox("server");
	System.out.println("starting up..");



	OtpErlangObject o;
	OtpErlangTuple msg;
	OtpErlangPid from;
	while (true) {
	    try {
		o = mbox.receive(); 
		if (o instanceof OtpErlangTuple) {
		    msg = (OtpErlangTuple)o; 
		    from = (OtpErlangPid)(msg.elementAt(0)); 
		    mbox.send(from,msg.elementAt(1));
		} 
	    }
	    catch (Exception e) {
		System.out.println("" + e);
	    }
	}

    }
 
}
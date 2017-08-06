class Message(p_id:String, p_msg:String) {
  val id  = p_id;
  val msg = p_msg;
}

object Message{
  def apply(id:String, msg:String) = new Message(id,msg)

  def unapply(m:Message) :Option[(String,String)] =  {
      if (m == null) None
      else  Some(m.id, m.msg)
  }
}

object EmailAddress {
  def apply(uname: String, dname: String) = uname + "@" + dname

  def unapply(str: String): Option[(String, String)] = {
      val parts = str split "@"
      if (parts.length == 2) Some(parts(0), parts(1)) else None
  }
}

def testMessagePattern(l:List[Message]):String = {
   l match {
     case Nil => "Not found"
     case Message(EmailAddress(u1,d1),_) ::Message(EmailAddress(u2,d2),_) :: _ if(u1==u2) => u1 + " got two successive emails"
     case h::t => testMessagePattern(t)
   }
}

val messageList = List(
Message("tom@gmail.com","Message text 1"),
Message("7742394590","Message text 2"),
Message("8326192398","Message text 3"),
Message("lisa@gmail.com","Message text 4"),
Message("lisa@yahoo.com","Message text 5"),
Message("harry@gmail.com","Message text 6")
)

println(testMessagePattern(messageList))

package prolog
object Main
{
	def main(args: Array[String])
	{	
		while(true)
		{
			print("\n?")
			var qry=readLine()
			qry=qry.replace("\n","");
			qry=qry.replace(" ","");
			if(qry(qry.length-1)=='?')
			{
				interpreter.infer(qry.substring(0,qry.length-2))	
			
			}
			else
			{
				interpreter.ruleDB=new rule(qry)::interpreter.ruleDB
			}
		}
	}
}

class term(s:String)
{
	private val f=s.split("[\\(\\),]")
	val pred=f.head
	val args=f.tail
}
class rule(s:String)
{
	private val f=s.split(":-")
	val head=new term(f(0))
	var subgoal:List[term]=List()
	if(f.length==2)
	for(i<-f(1).split(";"))
	{
		subgoal=new term(i)::subgoal
	}
}
class goal(val rule:rule,val parent:goal,e:scala.collection.mutable.Map[String,String])
{

	var env = interpreter.deepcopy(e)
	var inx=0
}

object interpreter
{	
	var ruleDB:List[rule]=List()
	var stack:List[goal]=List()
	
	
	def infer(qry:String):Unit=
	{	var skip=true
		//print( "search", qry)
		var targetgoal=new goal(new rule("got(goal):-x(y)"),null,scala.collection.mutable.Map())
		targetgoal.rule.subgoal=List(new term(qry))
		//print("stack")
		stack=targetgoal::stack
		while(! stack.isEmpty)
		{	skip=false
			var c=stack.head
			stack=stack.tail
			//print("pop")
			if(c.inx>=c.rule.subgoal.length)
			{
				if(c.parent==null)
				{
					if(! c.env.isEmpty)
						print(c.env)
					else 
						print("Yes");
					skip=true
				}
				if(!skip)
				{	var parent=duplicat(c.parent)
					unify(c.rule.head,c.env,parent.rule.subgoal(parent.inx),parent.env)
					parent.inx = parent.inx+1
					//print ("stack")
					stack=parent::stack
					skip=true
				}
			}
			if(!skip)
			{
				var subgoalterm = c.rule.subgoal(c.inx)
				for(crule<-ruleDB)
				{
					if(crule.head.pred != subgoalterm.pred)
						skip=true
					else if(crule.head.args.length != subgoalterm.args.length)
						skip=true
					if(!skip)
					{
						var child = new goal(crule, c,scala.collection.mutable.Map())
						var ans = unify(subgoalterm, c.env,crule.head, child.env)
						if(ans==1)
						{
							//print("stack")
							stack=child::stack
						}
					}
				}
			}
		}
	}
	def unify(st:term,se:scala.collection.mutable.Map[String,String],dt:term,de:scala.collection.mutable.Map[String,String]):Int=
	{	var sval=""
		var dval=""
		val nargs=st.args.length
		if(nargs != dt.args.length) return(0)
		else if(st.pred != dt.pred) return(0)
		
		for(i<-0 to nargs-1)
		{
			var sarg=st.args(i)
			var darg=dt.args(i)
			if(sarg <= "Z") 
			{
				if(! se.contains(sarg)) return(1)
				sval=se(sarg)
			}
			else 
			{
				sval=sarg
			}
			if(darg < "Z")
			{
				if(! de.contains(darg))
				{
					 de(darg)=sval
				}
				else
				{	dval=de(darg)
					if(dval != sval) return(0)
					
				}
				
			}
			else if(darg != sval) 0
		}
		return(1)
		
		
		
		
		
		
		
		
		
		
		
		
	}
	def deepcopy(l:scala.collection.mutable.Map[String,String]):scala.collection.mutable.Map[String,String]=
	{	
		var lt:scala.collection.mutable.Map[String,String]=scala.collection.mutable.Map()
		for(i<-l)
		{
			lt=lt+i
		}
		lt
	
	}
	def duplicat(g:goal):goal=
	{
		var newg=new goal(g.rule,g.parent,g.env)
		return(newg)
	}
}
object main
{

}

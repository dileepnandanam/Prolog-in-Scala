package prolog
object Main
{
	
	def main(args:Array[String])
	{
		while(true)
		{
			print("\n?")
			var qry=readLine()
			qry=qry.replace("\n", "");
			qry=qry.replace(" ", "");
			
			if(qry=="")
			{
			
			}
			else if(qry=="dump")
			{
				print(ip.rules,"\n")
			}
			else if(qry=="exit")
			{
				
			}
			else if(qry=="trace=0")
			{
				ip.trace=false
			}
			else if(qry=="trace=1")
			{
				ip.trace=true
			}
			else if(qry(qry.length-1)=='?')
			{
				ip.search(ip.term(qry.substring(0,qry.length-1),List()))
			
			}
			else
			{
				ip.rules=ip.Rule(qry)::ip.rules
			}
		}
	}
}


object ip
{	

	var trace:Boolean=false
	var indent:String=""
	var rules:List[Rule]=List()
	var queue:List[Goal]=List()
	case class term(var s:String, var args:List[term]) 
	{	var pred=""
		if(s != "")
		{	var parts=List[String]()
			
		
			if(args.isEmpty)
				parts = splitInfix(s)
			if(! args.isEmpty)			
			{	pred = s
				args = args
			}
			else if(! parts.isEmpty)
			{	args = parts.tail.map((ts:String)=>term(ts,List[term]()))
				pred = parts.head
			}
			else if(s(s.length-1) == ']')
			{   	var flds = split(s.substring(1,s.length-1),", ")
				var fld2 = split(s.substring(1,s.length-1),"|")
				if(fld2.length > 1)
				{
					args=fld2.map((ts:String)=>term(ts,List[term]()))
					pred = "."
				}
				else
				{
					flds=flds.reverse
					var l = term(".",List[term]())
					for( fld <- flds )
					{
						l = term(".",term(fld,List[term]())::List(l))
					}
					pred = l.pred
					args = l.args
			
				}
			}
			else if( s(s.length-1) == ')')
			{			 
				var flds = split(s,"(")
				if(flds.length != 2)
					print("syntax error")
		
				for(t <- split(flds(1).substring(0,flds(1).length-1),", "))
				{
					args =args:::List(term(t,List[term]()))
				}
				pred = flds(0)
			}
			else
			{ 
				pred = s		   
				args = List[term]()
			}
			
		}
		
		
		def printme()
		{
			if(args.isEmpty) print(pred)
			else
			{	
				print(pred)
				print("(")
				var ci=0
				for(i<-args)
				{	ci=ci+1
					i.printme()
					if(ci<args.length)
						print(", ")
				}
				print(")")
			
			}
		
		
		}
			
	}

	case class Goal(val rule:Rule,val parent:Goal,e:scala.collection.mutable.Map[String,term])
	{

		var env = ip.deepcopy(e)
		var inx=0
	}

	case class Rule(s:String)
	{
		private val f=s.split(":-")
		val head=new term(f(0),List[term]())
		var goals:List[term]=List()
		if(f.length==2)
		for(i<-split(f(1),", "))
		{
			goals=new term(i,List[term]())::goals
		}
	}




	
	def sts(ok:Int, why:String):Int=
	{
		indent = indent.substring(0,indent.length-2)
		if(trace)
		{
			print("\n")
			print(indent)
			print( List("No", "Yes")(ok))
			print( why)
		}
		return(ok)
		
	}
    
    
    
    
    
	def split(s:String,sep:String)=sp(s,sep,List[String](),0,0)
	def sp(e:String,s:String,l:List[String],i:Int,n:Int):List[String]={							     
		if(i+s.length>=e.length) l:::List(e)
		else if(e.substring(i,i+s.length)==s & n==0) sp(e.substring(i+s.length,e.length),s,l:::List(e.substring(0,i)),0,n)
		else if(e(i)=='[' | e(i)=='(') sp(e,s,l,i+1,n+1)
		else if(e(i)==']' | e(i)==')') sp(e,s,l,i+1,n-1)
		else sp(e,s,l,i+1,n)
	}	
	

	
	
	
		
		
	
	def deepcopy(l:scala.collection.mutable.Map[String,term]):scala.collection.mutable.Map[String,term]=
	{	
		var lt:scala.collection.mutable.Map[String,term]=scala.collection.mutable.Map()
		for(i<-l)
		{
			lt=lt+i
		}
		lt
	
	}
	def duplicatGoal(g:Goal):Goal=
	{
		var newg=Goal(g.rule,g.parent,deepcopy(g.env))
		return(newg)
	}



	def isVariable(term:term):Boolean =term.args.isEmpty & "QWERTYUIOPASDFGHJKLZXCVBNM".contains(term.pred.substring(0,1))

	def isConstant(term:term):Boolean =term.args.isEmpty & ! "QWERTYUIOPASDFGHJKLZXCVBNM".contains(term.pred.substring(0,1))





	val infixOps = List("*is*", "==", "<", ">", "+", "-", "*", "/")
	def splitInfix(s:String):List[String]=
	{
		var p=List[String]()
		for(i <- infixOps)
		{
			p=split(s,i)
			if(p.length >1) return(i::p)
			
		}
		return(List[String]())
	}	
	
	def add (a:term,b:term):term= term((a.pred.toInt+b.pred.toInt).toString,List[term]())
	def sub (a:term,b:term):term= term((a.pred.toInt-b.pred.toInt).toString,List[term]())
	def mul (a:term,b:term):term= term((a.pred.toInt*b.pred.toInt).toString,List[term]())
	def lt  (a:term,b:term):term= if(a.pred.toInt <  b.pred.toInt) term("true",List[term]()) else term("false",List[term]())
	def eq  (a:term,b:term):term= if(a.pred.toInt == b.pred.toInt) term("true",List[term]()) else term("false",List[term]())


	def eval (Term:term, env:scala.collection.mutable.Map[String,term]) :term =
	{	
		
		if(Term.pred=="+")
		{
			var special:(term,term) => term=add
			return special(eval(Term.args(0),env),eval(Term.args(1),env))
		}
		else if(Term.pred == "*")
		{
			var special:(term,term)=>term=mul
			return special(eval(Term.args(0),env),eval(Term.args(1),env))
		}
		else if(Term.pred == "==")
		{
			var special:(term,term)=>term=eq
			return special(eval(Term.args(0),env),eval(Term.args(1),env))
		}
		else if(Term.pred == "<")
		{
			var special:(term,term)=>term=lt
			return special(eval(Term.args(0),env),eval(Term.args(1),env))
		}
		else
		{
			if(isConstant(Term))
				return (Term)
			if(isVariable(Term))
			{	
		
				if(env.contains(Term.pred))
					return eval(env(Term.pred),env)
				else
					return(term("",List[term]()))
			}
			var args =List[term]()
			for(arg <- Term.args)
			{
				var a = eval(arg,env)
				a match 
				{
					case term("",List()) => return(term("",List[term]()))
				}
				args=args:::List(a)
			} 
		
			return term(Term.pred, args)
		}
		
	
		
		
			
	
	}
	def truthVal(term:term):Boolean= if(term.args.isEmpty) term.pred=="true" else true

	
	
	
	
	
	def unify (src:term, srcEnv:scala.collection.mutable.Map[String,term], dest:term, destEnv:scala.collection.mutable.Map[String,term]) :Int=
	{

		if(trace)
		{
			print("\n")
			print(indent)
			print("Unify")
			print(src)
			print(srcEnv)
			print( "to")
			print( dest)
			print( destEnv)
		}
		indent = indent+"  "
		if(src.pred == '_' | dest.pred == '_')
		{
			return sts(1,"Wildcard")
		}
		if(isVariable(src))
		{	
			var srcVal = eval(src, srcEnv)
		
			srcVal match 
			{
		
			case term("",List()) => return(sts(1,"Src unset"))
			case _ => return(sts(unify(srcVal,srcEnv,dest,destEnv), "Unify to Src Value"))
		}
		}
		if(isVariable(dest))
		{	
			var destVal = eval(dest, destEnv)
		
		
			destVal match
			{
				case term("",List()) => destEnv(dest.pred) = eval(src,srcEnv)
										return sts(1,"Dest updated 1")
				case _ => return sts(unify(src,srcEnv,destVal,destEnv),"Unify to Dest value")
			}	
		
			
			
			

		}
		else if(src.pred != dest.pred)
		{
			return(sts(0,"Diff predicates"))
		}
		else if(src.args.length != dest.args.length)
		{
			return(sts(0,"Diff // args"))
		}
		else
		{
			var dde = deepcopy(destEnv)
			for(i <- 0 to src.args.length-1)
			{	if(0 == unify(src.args(i),srcEnv,dest.args(i),dde))
					return(sts(0,"Arg doesn't unify"))
			}
			update(destEnv,dde)
			return(sts(1,"All args unify"))

		}
	}	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	def update(a:scala.collection.mutable.Map[String,term],b:scala.collection.mutable.Map[String,term]):Unit=
	{
		
		for(i <- b)
		{
			a.update(i._1,i._2)
		}
		
	}
	
	
	
	def search (term:term)
	{
		 
		 if(trace){
		  	
		  	print(term)
		 }
		 var goal = Goal(Rule("dummyPred(done):-x(y)"),null,scala.collection.mutable.Map[String,term]())
		 goal.rule.goals = List(term)
		 var queue = List(goal)
		 
		 
		 def search0():Unit=
		 {	if(queue.isEmpty) return()
		 		
			var c = queue.head
			queue=queue.tail
							 // Next goal to consider
			if(trace){
				print("\n")
				print("Deque")
				print( c)
			}
			
			if(c.inx >= c.rule.goals.length)		// Is this one finished?
			{	
				if(c.parent == null)			// Yes. Our original goal?
				{	 
					 if(!c.env.isEmpty) 
					 {	
					 	print("\n")
					 	print("{")
					 	
					 	for(ele<-c.env)
					 	{	
					 		print(ele._1)
					 		print("->")
					 		ele._2.printme()
					 		
					 			print(" ")
					 	}
					 	print("}")
					 			// Yes. tell user we have a solution
					 }
					 else{ 
					 	print("\n")
					 	print("Yes" )		 
					 }
					 return(search0())
				}
				var parent = duplicatGoal(c.parent)	// Otherwise resume parent goal
				unify(c.rule.head,	 c.env, parent.rule.goals(parent.inx),parent.env)
				parent.inx = parent.inx+1		 // advance to next goal in body
				queue=queue:::List(parent)			// let it wait its turn
				if(trace){ 
					print("\n")
					print("QueueFwrd")
					print( parent)
				}
				return(search0())
			}
			// No. more to do with this goal.
			var term = c.rule.goals(c.inx)			// What we want to solve
			
			var pred = term.pred				// Special term?
			if(List("*is*", "cut", "fail", "<", "==").contains(pred))
			{
				if(pred == "*is*" )
				{	
			var ques = eval(term.args(0),c.env)
				 var ans	= eval(term.args(1),c.env)
				 if(ques match {case ip.term("",List()) => true case _ => false} )
					c.env(term.args(0).pred) = ans	// Set variable
				 else if(ques.pred != ans.pred)
					return(search0())			 // Mismatch, fail
				}
				else if(pred == "cut") queue = List[Goal]() // Zap the competition
				else if(pred == "fail") return(search0())	// Dont succeed
				else if( eval(term,c.env) match {case ip.term("",List()) => true case _ => false} ) return(search0()) // Fail if not true
				c.inx = c.inx + 1			// Succeed. resume self.
				queue=queue:::List(c)
				return(search0())
			}
			
			
			for( rule <- rules )				 // Not special. Walk rule database
			{	
				
				
				if(rule.head.pred == term.pred & rule.head.args.length == term.args.length) 
				{
					var child = Goal(rule, c, scala.collection.mutable.Map[String,term]())			// A possible subgoal				
						
					var ans = unify (term, c.env, rule.head, child.env)
					if(ans==1 | ans==0){				// if unifies, queue it up
						queue=queue:::List(child)
					 	if(trace){ 
					 		print("\n")
					 		print( "Queue")
					 		print(child)
						
						}
					}
				}

			}
			return(search0())
			
	
	print("{")
		 }
		 return(search0())	
	
	
	



	}	
	
	
	
	
	
	




	
	
}




	

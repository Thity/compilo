program WhileLoopTry{
	println(new Operation().compute(10));

}

class Operation{
	def compute(n:Int): Int = {
	var res : Int;
	var zero : Int ;
	zero = 0;

	while (zero < n){
		res = res + n;
	}



	return res;
	}
}

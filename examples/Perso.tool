program Double {
        if (new CustomFloat().init(17).default(46).test() == 0) {println("OK");}
}

class CustomFloat {
	var integer : Int;
	var decimal : Int[];
	var precision : Int;
	
	def init(precision_ : Int) : CustomFloat = {
		precision = precision_;
		decimal = new Int[precision];
		return this;
	}
	
	def default(integer_ : Int) : CustomFloat = {
		var trashBin : Int;
		trashBin = this.setDecimal(0, 4);
		trashBin = this.setDecimal(1, 5);
		trashBin = this.setDecimal(2, 6);
		trashBin = this.setInteger(integer_);
		return this;
	}
	
	def getPrecision() : Int = {
		return precision;
	}
	
	def setInteger(i : Int) : Int = {
		integer = i;
		return i;
	}
	
	def setDecimal(i : Int, value : Int) : Int = {
		decimal[i] = value;
		return value;
	}
	
	def getDecimal(i : Int) : Int = {
		return decimal[i];
	}
	
	def getInteger() : Int = {
		return integer;
	}
	
	def add(operand : CustomFloat) : CustomFloat = {
		var i : Int;
		var result : CustomFloat;
		var reminder : Int;
		var temp : Int;
		result = new CustomFloat().init(precision);
		
		if (operand.getPrecision() == precision) {
			i = precision-1;
			reminder = 0;
			
			while (0-1 < i) {
				temp = decimal[i] + operand.getDecimal(i) + reminder;
				if (9 < temp) {
					reminder = 1;
					temp = result.setDecimal(i, temp - 10);
				} else {
					reminder = 0;
					temp = result.setDecimal(i, temp);
				}
				i = i-1;
			}
			
			temp = integer + operand.getInteger() + reminder;
			temp = result.setInteger(temp);
			
		} else {
			println("Invalid arg for add: must have the same precision.");
		}
		
		return result;
	}
	
	def toString() : String = {
		var i : Int;
		var result : String;
		
		result = integer + ".";
		
		i = 0;
		while (i < precision) {
			result = result + decimal[i];
			i = i + 1;
		}
		return result;
	}
	
	def test() : Int = {
		var c : CustomFloat;
		var r : CustomFloat;
		
		c = new CustomFloat().init(17).default(35);
		r = this.add(c);
		
		println(r.toString());
		return 0;
    }
}

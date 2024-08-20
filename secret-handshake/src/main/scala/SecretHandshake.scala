object SecretHandshake:
    def commands(n: Int): Seq[String] =
        if n >= 16 then hs(n - 16) else hs(n).reverse

    private def hs(n: Int): Seq[String] =
        if n == 0 then
            return Seq.empty
        val xs = Seq((8, "jump"), (4, "close your eyes"), (2, "double blink"), (1, "wink"))
        val (a, b) = xs.find(_._1 <= n) match
            case Some(x) => x
            case _ => (0, "")
        
        b +: hs(n - a)
        
    
    
suits <- c("Diamonds","Clubs","Hearts","Spades")
numbers <- c("Ace","Deuce","Three","Four","Five","Six","Seven","Eight","Nine","Ten",
             "Jack","Queen","King")
deck <- expand.grid(number = numbers, suit = suits)
deck <- paste(deck$number,deck$suit)
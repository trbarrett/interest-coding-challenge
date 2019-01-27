namespace InterestCodeChallenge

module Domain =
    open System
    
    //////////////////////////////////////////
    // *** Domain Model ***
    //////////////////////////////////////////

    [<Measure>] 
    type money
    
    // Use explicit types for the identifiers
    type InvestorId = | InvestorId of string
    type LoanId = | LoanId of string
    type TrancheName = | A | B
    // Use a compound identifier for tranche Id
    type TrancheId = | TrancheId of LoanId * TrancheName 
    type InvestmentId = | InvestmentId of Guid
    
    // Use expicit types for the type of errors we create
    type InvestmentErrors =
        // In real-world code we would want to annotate all of these with context to help
        // debug, I've added some basic context for CannotInvestZeroOrLess as an example
        | CannotInvestZeroOrLess of InvestorId * decimal<money>
        | InvestorDoesNotHaveEnoughMoneyToInvest
        | TrancheDoesNotHaveSpaceForInvestment
        | CannotInvestBeforeLoanStart
        | DuplicateInvestmentDetected
        
    // These models are kept to the minimum needed to model the test requirements/scenarios

    type Investor =
        { Id: InvestorId;
          Wallet: decimal<money> }
        
    type Tranche =
        { Id: TrancheId;
          MonthlyInterestRate: decimal
          Available: decimal<money>
          Invested: decimal<money> }
    
    type Loan =
        { Id: LoanId;
          Tranches: Map<TrancheName, Tranche>;
          StartDate: DateTime }
        
    type Investment =
        { Id: InvestmentId;
          InvestorId: InvestorId;
          Amount: decimal<money>;
          Date: DateTime;
          TrancheId: TrancheId; }
        
    type InterestPayment =
        { InvestmentId: InvestmentId
          Period: DateTime * DateTime
          Amount: decimal<money> }
        
    //////////////////////////////////////////
    // *** Application State Model ***
    //////////////////////////////////////////
        
    // We don't have a db, and everything is immutable. So there needs to be a
    // data structure to maintain the application state
    type WorldState =
        { Investors: Map<InvestorId, Investor>;
          Loans: Map<LoanId, Loan>;
          Investments: Map<InvestmentId, Investment>;
          InterestPayments: InterestPayment list}
        
    // The following are helper methods to work with the WorldState.
    // These would all be DB queries or similar in a real system
    // Note: I've used find instead of tryFind throughout just to keep the code simpler
    
    let getInvestor investorId state =
        state.Investors |> Map.find investorId
    
    let private updateInvestor investorId f state =
        let updatedInvestor = f (state |> getInvestor investorId)
        let investors = state.Investors |> Map.add investorId updatedInvestor 
        { state with Investors = investors }
        
    
    let getLoanAndTranche (TrancheId (loanId, trancheName)) state =
        let loan = state.Loans |> Map.find loanId
        let tranche = loan.Tranches |> Map.find trancheName
        (loan, tranche)
        
    let getTranche trancheId state =
        state |> getLoanAndTranche trancheId |> snd
        
    let private updateLoan loanId f state =
        let updatedLoan = f (state.Loans |> Map.find loanId)
        let loans = state.Loans |> Map.add loanId updatedLoan 
        { state with Loans = loans }
        
    let private updateTranche trancheId f state =
        let (TrancheId (loanId, trancheName)) = trancheId
        let (_, tranche) = state |> getLoanAndTranche trancheId
        let tranche = f tranche
        state |> updateLoan loanId (fun loan ->
            { loan with Tranches = loan.Tranches |> Map.add trancheName tranche } )
        
    let getInvestment investmentId state =
        state.Investments |> Map.find investmentId
        
    let newInvestment investorId amount trancheId date =
        { Id = InvestmentId (Guid.NewGuid())
          InvestorId = investorId
          TrancheId = trancheId
          Amount = amount
          Date = date }
        
    let private addInvestment investment state =
        if (state.Investments |> Map.containsKey investment.Id) then
           Error DuplicateInvestmentDetected 
        else 
            Ok { state with Investments = (state.Investments |> Map.add investment.Id investment) }
            
    let getInterestPaymentAmount investmentId period state =
        state.InterestPayments
        |> List.find (fun ip -> ip.InvestmentId = investmentId && ip.Period = period)
        |> (fun x -> x.Amount)
            
    let private addInterestPayment interestPayment state =
        { state with InterestPayments = interestPayment::state.InterestPayments }
        
    
    // Note: I've skipped doing add Loan, add Investor etc. They're not needed
    // for the scenario and all fairly trivial to implement in any event
        
    //////////////////////////////////////////
    // *** Logic for the Challenge ***
    //////////////////////////////////////////
    
    let private adjustInvestorWallet investorId amount state =
        state |> updateInvestor investorId (fun investor ->
            { investor with Wallet = investor.Wallet + amount })
    
    let private adjustTrancheInvested trancheId amount state =
        state |> updateTranche trancheId (fun tranche ->
            { tranche with
                Available = tranche.Available - amount
                Invested = tranche.Invested + amount})
        
    let makeInvestment investment state =
        let investor = state |> getInvestor investment.InvestorId
        let (loan, tranche) = state |> getLoanAndTranche investment.TrancheId
        
        if (investment.Amount <= 0m<money>) then
            Error (CannotInvestZeroOrLess (investor.Id, investment.Amount))
        elif (investment.Amount > investor.Wallet) then
            Error InvestorDoesNotHaveEnoughMoneyToInvest
        elif (investment.Amount > tranche.Available) then
            Error TrancheDoesNotHaveSpaceForInvestment
        elif (investment.Date < loan.StartDate) then
            Error CannotInvestBeforeLoanStart
        elif (state.Investments |> Map.containsKey investment.Id) then
            Error DuplicateInvestmentDetected
        else
            state
            |> adjustInvestorWallet investment.InvestorId -investment.Amount
            |> adjustTrancheInvested investment.TrancheId investment.Amount
            |> addInvestment investment
    
    let convertMonthlyToDailyInterestRate monthlyInterestRate =
        monthlyInterestRate * 12.0m / 365.0m

    let calculateInterestForInvestment investment (periodStart: DateTime) (periodEnd: DateTime) state =
        let (loan, tranche) = state |> getLoanAndTranche investment.TrancheId
        let dailyInterestRate = convertMonthlyToDailyInterestRate tranche.MonthlyInterestRate 
        let calcStartDate = List.max [investment.Date; periodStart; loan.StartDate]
        let days = floor (periodEnd.Subtract(calcStartDate).TotalDays + 1.0)
        if days <= 0.0 then
            // For this to happen the investment must have started after the
            // period. The investor has therefore earned 0 interest
            0m<money>
        else
            // Note we don't round here
            investment.Amount * dailyInterestRate * (decimal days)
            
    let round2pennies (amount: decimal<'u>) : decimal<'u> =
        let f = (fun x -> Decimal.Round(x, 2))
        decimal amount |> f |> LanguagePrimitives.DecimalWithMeasure
        
    let produceInterestForInvestments (periodStart, periodEnd) state =
        // We could do some work to prevent duplicate interest payments for investments
        // with a given interest period, but it's beyond the scope of this code challenge
        (state, state.Investments) ||> Map.fold (fun state _ investment ->
            let interest = calculateInterestForInvestment investment periodStart periodEnd state
            let interest = round2pennies interest
            let interestPayment = { InvestmentId = investment.Id; Period = (periodStart, periodEnd); Amount = interest }
            state
            |> adjustInvestorWallet investment.InvestorId interest
            |> addInterestPayment interestPayment )

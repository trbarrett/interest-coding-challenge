module InterestCodeChallenge.TestFixtures

open System
open InterestCodeChallenge.Domain

let londonLoanId = LoanId "London"
let londonTrancheA = 
    { Id = TrancheId (londonLoanId, A)
      MonthlyInterestRate = 0.009m
      Available = 100_000m<money>
      Invested = 0m<money> }
let londonTrancheB =
    { Id = TrancheId (londonLoanId, B)
      MonthlyInterestRate = 0.02m
      Available = 500m<money>
      Invested = 0m<money> }

let londonLoan =
    { Id = londonLoanId
      StartDate = DateTime(2018, 06, 20)
      Tranches = Map [ (A, londonTrancheA); (B, londonTrancheB) ] }

let bristolLoanId = LoanId "Bristol"
let bristolTrancheA = 
     { Id = TrancheId (bristolLoanId, A)
       MonthlyInterestRate = 0.011m
       Available = 60_000m<money>
       Invested = 0m<money> }

let bristolLoan =
    { Id = bristolLoanId
      StartDate = DateTime(2018, 09, 05)
      Tranches = Map [ (A, bristolTrancheA) ] }

let richInvestor = { Id = InvestorId "Rich";  Wallet = 10_000.00m<money> }
let poorInvestor = { Id = InvestorId "Poor";  Wallet = 500.00m<money> }
    
let testState =
    { Investors = Map [
        (richInvestor.Id, richInvestor)
        (poorInvestor.Id, poorInvestor) ]
      Loans = Map [
        (londonLoan.Id, londonLoan) 
        (bristolLoan.Id, bristolLoan) ]
      Investments = Map []
      InterestPayments = []}
         
let defaultInvestment =
    { Id = InvestmentId (Guid.NewGuid())
      InvestorId = richInvestor.Id
      TrancheId = londonTrancheA.Id
      Amount = 500m<money>
      Date = DateTime(2018, 10, 01) }

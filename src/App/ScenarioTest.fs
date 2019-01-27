module InterestCodeChallenge.ScenarioTest

open System
open Expecto
open Expecto.Flip
open InterestCodeChallenge.Domain
open InterestCodeChallenge.UnitTests

[<Tests>]
let test =
    testCase "scenario." <| fun _ ->
        let loanId = (LoanId "TestLoan")
        let trancheA =
            { Id = TrancheId (loanId, A)
              MonthlyInterestRate = 0.03m
              Available = 1000m<money>
              Invested = 0m<money>  }
        let trancheB =
            { Id = TrancheId (loanId, B)
              MonthlyInterestRate = 0.06m
              Available = 1000m<money>
              Invested = 0m<money> }
        let loan =
            { Id = (LoanId "TestLoan");
              StartDate = DateTime(2015, 10, 01);
              Tranches = Map [ (A, trancheA); (B, trancheB) ] }
            
        let investor1 = { Id = (InvestorId "Investor 1"); Wallet = 1000m<money> }
        let investor2 = { Id = (InvestorId "Investor 2"); Wallet = 1000m<money> }
        let investor3 = { Id = (InvestorId "Investor 3"); Wallet = 1000m<money> }
        let investor4 = { Id = (InvestorId "Investor 4"); Wallet = 1000m<money> }
        
        // setup the initial state of the applciation
        let state =
            { Loans = Map [(loanId, loan)]
              Investors = [ investor1; investor2; investor3; investor4 ] |> List.map (fun x -> x.Id, x) |> Map.ofList
              Investments = Map []
              InterestPayments = [] }
        
        
        // As “Investor 1” I’d like to invest 1,000 pounds on the tranche “A” on 03/10/2015: “ok”
        let investment1 = newInvestment investor1.Id
                                        1000m<money>
                                        trancheA.Id
                                        (DateTime(2015, 10, 03))
        let state = state |> makeInvestment investment1 |> assertOk
        
        // As “Investor 2” I’d like to invest 1 pound on the tranche “A” on 04/10/2015: “exception”.
        let investment2 = newInvestment investor2.Id
                                        1m<money>
                                        trancheA.Id
                                        (DateTime(2015, 10, 04))
        state |> makeInvestment investment2
        |> Expect.equal "" (Error TrancheDoesNotHaveSpaceForInvestment)
        
        // As “Investor 3” I’d like to invest 500 pounds on the tranche “B” on 10/10/2015: “ok”
        let investment3 = newInvestment investor3.Id
                                        500m<money>
                                        trancheB.Id
                                        (DateTime(2015, 10, 10))
        let state = state |> makeInvestment investment3 |> assertOk
        
        // As “Investor 4” I’d like to invest 1,100 pounds on the tranche “B” 25/10/2015: “exception”
        let investment4 = newInvestment investor4.Id
                                        1100m<money>
                                        trancheB.Id
                                        (DateTime(2015, 10, 25))
        state |> makeInvestment investment4
        |> Expect.equal "" (Error InvestorDoesNotHaveEnoughMoneyToInvest)
        
        // Trance availbility and invested amounts should all be updated
        getTranche trancheA.Id state
        |> (fun t -> t.Available, t.Invested)
        |> Expect.equal "" ( 0m<money>, 1000m<money>)
        
        getTranche trancheB.Id state
        |> (fun t -> t.Available, t.Invested)
        |> Expect.equal "" ( 500m<money>, 500m<money>)

        
        // On 01/11/2015 the system runs the interest calculation for the period 01/10/2015 -> 31/10/2015:
        let interestPeriod = ((DateTime(2015, 10, 01)), (DateTime(2015, 10, 31)))
        let state = state |> produceInterestForInvestments interestPeriod
                                
                                
        // Interest payments for each of the 2 valid investments should have been created
        state.InterestPayments |> List.length |> Expect.equal "" 2
        
        // “Investor 1” earns 28.06 pounds after investing all their money
        getInvestor investor1.Id state
        |> Expect.equal "" { investor1 with Wallet = 28.60m<money> }
        
        getInterestPaymentAmount investment1.Id interestPeriod state
        |> Expect.equal "" 28.60m<money>
        
        // "Investor 2" is unchagned
        getInvestor investor2.Id state |> Expect.equal "" investor2
        
        // “Investor 3” earns 21.29 pounds after investing half their money
        getInvestor investor3.Id state
        |> Expect.equal "" { investor3 with Wallet = 500m<money> + 21.70m<money> }
        
        getInterestPaymentAmount investment3.Id interestPeriod state
        |> Expect.equal "" 21.70m<money>
        
        // "Investor 4" is unchagned
        getInvestor investor4.Id state |> Expect.equal "" investor4
        

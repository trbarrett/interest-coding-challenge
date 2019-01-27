module InterestCodeChallenge.UnitTests

open System
open Expecto
open Expecto.Flip
open InterestCodeChallenge.Domain
open InterestCodeChallenge.TestFixtures

// test helper to assert the result was ok and pass the result on
let assertOk result =
    match result with
    | Ok value -> value
    | Error msg -> failwithf "Result had error: %A" msg
  
[<Tests>]
let tests =
    testList "InterestCodeChallenge unit tests." [
        testList "addInvestment." [
            testCase "should fail to add negative amount" <| fun _ ->
                testState
                |> makeInvestment { defaultInvestment with Amount = -5m<money> }
                |> Expect.equal "" (Error (CannotInvestZeroOrLess (defaultInvestment.InvestorId, -5m<money>)))
                
            testCase "should fail if investor does not have enough money" <| fun _ ->
                testState
                |> makeInvestment { defaultInvestment with
                                        InvestorId = poorInvestor.Id;
                                        Amount = 1000m<money> }
                |> Expect.equal "" (Error InvestorDoesNotHaveEnoughMoneyToInvest)
                
            testCase "should fail if investment is too large for the tranche" <| fun _ ->
                testState
                |> makeInvestment { defaultInvestment with
                                        TrancheId = londonTrancheB.Id;
                                        Amount = 1000m<money> }
                |> Expect.equal "" (Error TrancheDoesNotHaveSpaceForInvestment)
                
            testCase "should fail if investment date is earlier than the loan start" <| fun _ ->
                testState
                |> makeInvestment { defaultInvestment with Date = DateTime(2018, 01, 01) }
                |> Expect.equal "" (Error CannotInvestBeforeLoanStart)
                
            testCase "should fail if an investment gets duplicated somehow" <| fun _ ->
                testState
                |> makeInvestment defaultInvestment
                |> Result.bind (makeInvestment defaultInvestment)
                |> Expect.equal "" (Error DuplicateInvestmentDetected)
                
            testCase "should reduce tranche availability after investment" <| fun _ ->
                testState
                |> makeInvestment { defaultInvestment with TrancheId = londonTrancheA.Id }
                |> assertOk
                |> getTranche londonTrancheA.Id
                |> Expect.equal "" { londonTrancheA with
                                       Available = londonTrancheA.Available - defaultInvestment.Amount
                                       Invested = defaultInvestment.Amount }
                
            testCase "should reduce investor wallet after investment" <| fun _ ->
                testState
                |> makeInvestment { defaultInvestment with InvestorId = richInvestor.Id }
                |> assertOk
                |> getInvestor richInvestor.Id
                |> Expect.equal "" { richInvestor with
                                       Wallet = richInvestor.Wallet - defaultInvestment.Amount }
                
            testCase "should add investment to investments" <| fun _ ->
                testState
                |> makeInvestment defaultInvestment
                |> assertOk
                |> (fun x -> x.Investments |> Map.find defaultInvestment.Id)
                |> Expect.equal "" defaultInvestment
                
            testCase "should be able to invest in 2 loans" <| fun _ ->
                let investment1 = newInvestment richInvestor.Id 500m<money> londonTrancheB.Id (DateTime(2018, 10, 08))
                let investment2 = newInvestment richInvestor.Id 500m<money> bristolTrancheA.Id (DateTime(2018, 10, 08))
                
                let newState =
                    testState
                    |> makeInvestment investment1
                    |> Result.bind  (makeInvestment investment2)
                    |> assertOk
                    
                newState |> getInvestment investment1.Id
                |> Expect.equal "" investment1
                
                newState |> getInvestment investment2.Id
                |> Expect.equal "" investment2
                
                newState
                |> getInvestor richInvestor.Id
                |> Expect.equal "" { richInvestor with
                                       Wallet = richInvestor.Wallet - investment1.Amount - investment2.Amount }
                
                newState
                |> getTranche londonTrancheB.Id
                |> Expect.equal "" { londonTrancheB with
                                       Available = londonTrancheB.Available - investment1.Amount
                                       Invested = londonTrancheB.Invested + investment1.Amount}
                
                newState
                |> getTranche bristolTrancheA.Id
                |> Expect.equal "" { bristolTrancheA with
                                       Available = bristolTrancheA.Available - investment1.Amount
                                       Invested = bristolTrancheA.Invested + investment1.Amount}
        ]
        
        testList "calculateInterestForInvestment." (
            let londonTrancheADailyInterestRate =
               convertMonthlyToDailyInterestRate londonTrancheA.MonthlyInterestRate
            [
            testCase "should return 0 if invested after period start" <| fun _ ->
                testState
                |> calculateInterestForInvestment
                    { defaultInvestment with Date = DateTime(2018, 07, 01) }
                    (DateTime(2018, 06, 01))
                    (DateTime(2018, 06, 30))
                |> Expect.equal "" 0m<money>
                
            testCase "should return a single day's interest, if invested for one day" <| fun _ ->
                testState
                |> calculateInterestForInvestment
                    { defaultInvestment with Date = DateTime(2018, 06, 30) }
                    (DateTime(2018, 06, 01))
                    (DateTime(2018, 06, 30))
                |> Expect.equal "" (defaultInvestment.Amount * londonTrancheADailyInterestRate)
                
            testCase "should return two day's interest, if invested for two days" <| fun _ ->
                testState
                |> calculateInterestForInvestment
                    { defaultInvestment with Date = DateTime(2018, 06, 29) }
                    (DateTime(2018, 06, 01))
                    (DateTime(2018, 06, 30))
                |> Expect.equal "" (defaultInvestment.Amount * londonTrancheADailyInterestRate * 2m)
                
            testCase "should return a whole months worth of interest" <| fun _ ->
                testState
                |> calculateInterestForInvestment
                    { defaultInvestment with Date = DateTime(2018, 06, 01) }
                    (DateTime(2018, 07, 01))
                    (DateTime(2018, 07, 31))
                |> Expect.equal "" (defaultInvestment.Amount * londonTrancheADailyInterestRate * 31m)
                
            testCase "should calcualte interest from the start of the loan if started during period" <| fun _ ->
                testState
                |> calculateInterestForInvestment
                    // the loan started on the 20th, so there should only be 11 days of interest
                    { defaultInvestment with TrancheId = londonTrancheA.Id; Date = DateTime(2018, 06, 01) }
                    (DateTime(2018, 06, 01))
                    (DateTime(2018, 06, 30))
                |> Expect.equal "" (defaultInvestment.Amount * londonTrancheADailyInterestRate * 11m)
        ])
        
    ]
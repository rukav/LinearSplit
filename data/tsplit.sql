SELECT opt_tran.acid, count (*) * (datediff(dd, min(opt_tran.pfdate), '20100108') + 1)
FROM
opr_acctmodel, opt_tran
where
opt_tran.acid = opr_acctmodel.acid and opt_tran.effdate <= '20100108' 
and opt_tran.effdate >= opr_acctmodel.incpdate and opt_tran.effdate < opr_acctmodel.termdate
group by opt_tran.acid order by opt_tran.acid
go
        


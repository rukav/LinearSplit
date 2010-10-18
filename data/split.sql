SELECT DISTINCT link.acid FROM opr_acctmodel  link
WHERE EXISTS (select 1 from opt_tran tx
              where tx.acid = link.acid and tx.effdate <= '20100108' and
                    tx.effdate >= link.incpdate and tx.effdate < link.termdate )
UNION ALL SELECT DISTINCT link.acid FROM opr_acctmodel link
        inner join pr_account acct ON ( acct.acm_aco_id = link.acid )
WHERE  EXISTS (select 1 from opt_tran tx
                where tx.acid = acct.acid and tx.effdate <= '20100108' and tx.effdate >= link.incpdate and 
                      tx.effdate < link.termdate )
UNION ALL SELECT DISTINCT link.acid FROM opr_acctmodel link 
      inner join opp_grp_member grp ON (
            grp.grpid = link.acid AND
            grp.memtype = 1)
WHERE link.calctype not in ('M', 'E', 'B') AND EXISTS (select 1 from opt_tran tx
            where tx.acid = grp.memid AND
                      tx.effdate <= '20100108' AND
                      tx.effdate >= grp.incldate AND
                      tx.effdate < grp.excldate AND
                      tx.effdate >= link.incpdate AND
                      tx.effdate < link.termdate)
ORDER BY acid


select opt_tran.acid, count (*) from opr_acctmodel, opr_modelnode, opr_mdlnoderetccy, opt_tran 
where (opr_acctmodel.mdlid = opr_modelnode.mdlid and opr_mdlnoderetccy.mnc_mdn_mdlid=opr_modelnode.mdlid and
  opr_mdlnoderetccy.mnc_mdn_ndid = opr_modelnode.ndid) and (opr_acctmodel.acid=opt_tran.acid and
  opr_acctmodel.calctype not in ('B','E','M','L','0','1','2','3','4','5','6','7','8','9'))
group by opt_tran.acid order by opt_tran.acid
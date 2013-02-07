def column(matrix, i):
    return [row[i] for row in matrix]

def auc(ans,prob):
	N=len(prob)
	N_pos=sum(ans)
	ans=[ans for (prob,ans) in sorted(zip(prob,ans),reverse=True)]
	cs=[sum(ans[:i]) for i in range(1,len(ans)+1)]
	above=[x-y for (x,y) in zip(list(range(1,N+1)),cs)]
	return (1-sum([x*y for (x,y) in zip(above,ans)])/(N_pos*(N-N_pos)))

def ensemble(pool,ans,smp,maxn,nc,nr,cind,cumcsn,replacement=True,cross=5,maxauc=0):
	tls=list(range(1,maxn+1))[:]
	for tt in tls:
		maxi=0
		topflag=True
		ncind=len(cind)
		
		ils=list(range(1,nc+1))[:]
		for i in ils:
			if avail[i]:
				thismodel=[row[i] for row in pool]
				tmpprob=[a+b for (a,b) in zip(thismodel,cumcsn)]
				tmpprob=[a/(ncind+1) for a in tmpprob]
				tmpauc=[]
				
				iils=list(range(1,cross+1))[:]
				for ii in iils:
					tans=[ans[i] for i in smp[ii]]
					tprob=[tmpprob[i] for i in smp[ii]]
					tmpauc=tmpauc+auc(tans,tprob)
				
				tmpauc=sum(tmpauc)/len(auc)
				
				if tmpauc>maxauc:
					topflag=False
					maxauc=tmpauc
					maxi=i
		#for
		if not topflag:
			cind=cind+maxi
			maxmodel=[row[maxi] for row in pool]
			cumcsn=[a+b for (a,b) in zip(maxmodel,cumcsn)]
			avail[maxi]=False
		else:
			tmpcsn=[a/ncind for a in cumcsn]
			tmpauc=[]
			for i in range(1,cross+1):
				tans=[ans[i] for i in smp[i]]
				tcsn=[tmpcsn[i] for i in smp[i]]
				tmpauc=tmpauc+auc(tans,tcsn)
			if sum(avail)==nc:
				return([cind,tmpauc])
			if rplacement:
				avail=[True for i in range(1,nc+1)]
			else:
				return([cind,tmpauc])

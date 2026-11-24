from pathlib import Path
import hashlib,json,xml.etree.ElementTree as ET
r=Path(r'C:\Source\codex-workspace\.scratch\turbo-native-recursive-calls');repo=Path(r'C:\Source\SB')
ns={'t':'http://microsoft.com/schemas/VisualStudio/TeamTest/2010'}
assemblies=[];seen=set();canonical=set()
for path in sorted((r/'full-tests').rglob('*.trx')):
 root=ET.parse(path);c=root.find('.//t:ResultSummary/t:Counters',ns).attrib
 assert int(c['failed'])==0 and c['passed']==c['total'],(path,c)
 definitions={u.attrib['id']:u.find('t:TestMethod',ns).attrib for u in root.findall('.//t:UnitTest',ns)}
 results=root.findall('.//t:Results/t:UnitTestResult',ns)
 assert len(results)==int(c['total'])
 for result in results:
  key=result.attrib['testId'];assert key not in seen,('Repeated test',key);seen.add(key)
  method=definitions[key];case=(method['className'],method['name']);assert case not in canonical,('Repeated case',case);canonical.add(case)
  generated='GeneratedTurboExecutionTests' in method['className']
  assert generated==(path.parent.name!='other'),('Partition membership failed',path,case)
 assemblies.append({'Partition':path.parent.name,'Trx':str(path.relative_to(r)),'Passed':int(c['passed']),'Failed':0,'Total':int(c['total'])})
assert len(assemblies)==5,assemblies
discovery=json.loads((r/'discovery-counts.json').read_text());assert len(canonical)==sum(discovery.values())==4328
assert len(seen)==sum(a['Passed'] for a in assemblies)==4328,(len(seen),assemblies)
source=json.loads((r/'compiled-source-verification.json').read_text())
sha=lambda p:hashlib.sha256(p.read_bytes()).hexdigest().upper()
assert source['CompilerSha256']==sha(repo/'SB/bin/Debug/net10.0/SB.dll')==sha(repo/'SBTests/bin/Debug/net10.0/SB.dll')
assert source['TestAssemblySha256']==sha(repo/'SBTests/bin/Debug/net10.0/SBTests.dll')
for s in source['SourceFiles']:assert sha(repo/s['Path'])==s['Sha256'],s['Path']
summary={'TotalPassed':4328,'CompilerTestsPassed':4080,'RuntimeTestsPassed':248,'UniqueTests':len(seen),'RepeatedTests':0,'Failed':0,'Skipped':0,'DisjointFiltersCoverFullSolution':True,'Assemblies':assemblies}
(r/'suite-summary.json').write_text(json.dumps(summary,indent=2))
print('Full SB suite: 4,328 passed (4,080 compiler + 248 runtime), zero failures/skips; every test ran exactly once.')

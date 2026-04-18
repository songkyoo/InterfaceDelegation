# InterfaceDelegation Working Context

이 문서는 이후 작업에서 빠르게 맥락을 복구하기 위한 프로젝트 운영 메모다. 코드 변경 전에 이 문서를 먼저 확인하면 구조와 주의점을 빠르게 떠올릴 수 있다.

## 한 줄 요약

- 이 저장소는 `Expose`/`Lift` 특성을 기반으로 위임 코드를 생성하는 C# Roslyn incremental source generator 프로젝트다.
- 배포 패키지 `Macaron.InterfaceDelegation`는 실제 attribute 정의가 있는 `Core`와 소스 생성기 `Generator`를 함께 패킹한다.
- 현재 기준 테스트는 `40`개 모두 통과한다.
- 배포되는 세 프로젝트의 버전은 루트 `Directory.Build.props`에서 공용으로 관리한다.

## 솔루션 구성

- `InterfaceDelegation.Core`
  - 사용자 코드에서 참조하는 public API.
  - `ExposeAttribute`, `LiftAttribute`, `ImplementationMode` 정의가 있다.
  - 대상 프레임워크는 `netstandard2.0`.
- `InterfaceDelegation.Generator`
  - 실제 코드 생성기 구현.
  - `IIncrementalGenerator` 기반.
  - Roslyn 패키지 `Microsoft.CodeAnalysis.CSharp 4.3.0` 사용.
  - 대상 프레임워크는 `netstandard2.0`.
- `InterfaceDelegation`
  - NuGet 패키징 전용 프로젝트.
  - `Core` DLL은 `lib/netstandard2.0`와 `analyzers/dotnet/cs` 둘 다에 포함된다.
  - `Generator` DLL은 `analyzers/dotnet/cs`에 포함된다.
  - 공용 버전을 받아 패키지 버전도 함께 결정된다.
- `InterfaceDelegation.Tests`
  - NUnit 기반 생성 결과 검증 프로젝트.
  - 대상 프레임워크는 `net9.0`.
  - 생성 코드를 문자열 비교로 직접 검증한다.

## 핵심 파일

- [InterfaceDelegationGenerator.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\InterfaceDelegationGenerator.cs)
  - incremental generator 진입점.
  - 후보 수집, generation context 수집, 타입별 source output 조립을 담당한다.
- [GenerationContextFactory.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\GenerationContextFactory.cs)
  - attribute를 읽어 `GenerationInterfaceContext` 또는 `GenerationLiftContext`로 바꾼다.
  - `Expose`/`Lift` 사용 오류 진단도 여기서 만든다.
- [DelegationGenerationPipeline.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\DelegationGenerationPipeline.cs)
  - `Expose`와 `Lift`를 별도 생성 경로로 디스패치하는 파이프라인.
- [DelegationGenerationContext.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\DelegationGenerationContext.cs)
  - 생성 단계에서 공통으로 필요한 실행 상태를 묶는다.
- [ExposeGenerationPolicy.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\ExposeGenerationPolicy.cs)
  - `Expose` 전용 멤버 선택, 구현 충돌 판정, 접근 제한자/명시적 구현 규칙을 담는다.
- [LiftGenerationPolicy.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\LiftGenerationPolicy.cs)
  - `Lift` 전용 멤버 선택, 필터/리네임/base type 규칙, 충돌 판정을 담는다.
- [ExposeRenderingPolicy.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\ExposeRenderingPolicy.cs)
  - `Expose`에서 렌더링할 멤버 종류와 스킵 조건을 결정한다.
- [LiftRenderingPolicy.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\LiftRenderingPolicy.cs)
  - `Lift`에서 렌더링할 멤버 종류와 스킵 조건을 결정한다.
- [DelegationRenderingCore.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\DelegationRenderingCore.cs)
  - 메서드/프로퍼티/이벤트의 공통 문자열 렌더링 코어.
- [DelegationMemberUtilities.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\DelegationMemberUtilities.cs)
  - 멤버 열거, base type 순회, 구현 비교 같은 공통 저수준 유틸.
- [MemberComparisonHelpers.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\MemberComparisonHelpers.cs)
  - 이미 구현된 멤버인지 판별하는 비교 로직.
  - 암시적 구현과 명시적 구현을 분리해서 추적한다.
- [MethodSignatureGenerationHelpers.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\MethodSignatureGenerationHelpers.cs)
  - 파라미터 문자열, 인자 전달 문자열, 제네릭 제약, 기본값 표현식 생성.
- [SourceGenerationHelpers.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\SourceGenerationHelpers.cs)
  - 파일 헤더, partial 타입 선언 문자열, hint name 생성.
- [InterfaceDelegationGeneratorTests.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Tests\InterfaceDelegationGeneratorTests.cs)
  - 전체 생성기 계약이 모여 있는 사실상의 사양서.
- [Directory.Build.props](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\Directory.Build.props)
  - 패키징되는 프로젝트들의 공용 버전(`InterfaceDelegationVersion`)을 중앙 관리한다.

## 생성기 동작 흐름

1. `Initialize`에서 attribute가 붙은 field/property/primary-constructor parameter를 후보로 찾는다.
2. `GenerationContextFactory.Create`가 각 attribute를 읽어 `Expose` 또는 `Lift`용 context로 변환한다.
3. 잘못된 `Expose`/`Lift` 사용은 즉시 diagnostic으로 바꾼다.
4. containing type 기준으로 context를 묶은 뒤, `DelegationGenerationPipeline.Generate`가 각 context별 코드를 생성한다.
5. `DelegationGenerationPipeline` 내부에서
   - `ExposeGenerationPolicy` 또는 `LiftGenerationPolicy`가 대상 멤버와 생성 규칙을 결정한다.
   - `ExposeRenderingPolicy` 또는 `LiftRenderingPolicy`가 렌더링할 멤버를 고른다.
   - `DelegationRenderingCore`가 공통 코드 문자열을 만든다.
6. `AddSource`가 namespace, 중첩 타입, partial 타입 래퍼를 포함한 `.g.cs`를 만든다.

## Expose 규칙

- 대상은 non-generic interface 혹은 bound generic interface여야 한다.
- attribute 첫 번째 인자를 생략하면 멤버 자신의 타입을 인터페이스 타입으로 본다.
- 적용 대상은 field, reference-type property, primary constructor parameter다.
- 값 타입 property에 `Expose`를 붙이면 `MAID0002` 진단이 발생한다.
- 같은 containing type 안에서 같은 인터페이스를 두 번 위임하면 `MAID0003` 진단이 발생한다.
- 대상 멤버 타입이 지정한 인터페이스의 메서드/프로퍼티/이벤트를 시그니처까지 맞게 구현하지 않으면 `MAID0005` 진단이 발생하고 생성이 중단된다.
- `ImplementationMode.Implicit`
  - public 멤버 생성.
  - 이미 암시적 또는 명시적으로 구현된 멤버가 있으면 건너뛴다.
- `ImplementationMode.Explicit`
  - 명시적 인터페이스 구현 생성.
  - 이미 명시적으로 구현된 멤버만 중복으로 간주한다.
- 타입이 해당 인터페이스를 실제로 구현하는 concrete field일 때는, 명시적 인터페이스 구현 멤버 접근을 위해 local generic helper를 생성하는 경로가 있다.

## Lift 규칙

- 대상 타입의 public/internal method/property를 현재 타입으로 리프팅한다.
- indexer는 `Lift` 대상에서 제외된다.
- `Expose`와 달리 값 타입 property도 허용된다.
- 옵션
  - `includeBaseTypes`: base type 멤버까지 포함.
  - `filter`: 포함할 멤버 이름만 허용.
  - `remove`: 필터 적용 후 제외할 멤버.
  - `rename`: `기존이름:새이름`.
- `filter`/`remove`/`rename`에 존재하지 않는 멤버명을 넣으면 `MAID0004` 경고가 문자열 인자 위치에 발생한다.
- `Lift`는 암시적 멤버 충돌만 본다.
- 이름 변경 후 시그니처 기준으로 기존 멤버와 충돌하면 생성하지 않는다.

## 구현상 눈여겨볼 포인트

- 이벤트 위임 지원이 이미 구현되어 있다.
  - README에는 과거 경고 문구가 남아 있을 수 있으므로 실제 코드와 문서를 비교해서 판단해야 한다.
- base class에 추상 멤버가 있으면 `override`를 생성한다.
- 현재 타입 자체에 abstract 멤버가 있으면 새 구현을 만들지 않고 건너뛴다.
- 현재 구조는 크게 네 층으로 나뉜다.
  - context 생성: `GenerationContextFactory`
  - 생성 파이프라인: `DelegationGenerationPipeline`, `DelegationGenerationContext`
  - 정책: `ExposeGenerationPolicy`, `LiftGenerationPolicy`, `ExposeRenderingPolicy`, `LiftRenderingPolicy`
  - 공통 저수준/렌더링 코어: `DelegationMemberUtilities`, `DelegationRenderingCore`
- `Expose` 계약 검증은 context 생성 단계에서 먼저 수행된다.
  - 여기서 실패하면 생성 코드를 내지 않고 진단만 보고한다.
- `MemberComparisonHelpers`는 이제 중복 구현 판정뿐 아니라 `Expose` 대상 타입이 인터페이스 계약을 만족하는지도 검사한다.
- property/indexer/event/method는 공통 렌더링 코어에서 다루지만, 어떤 멤버를 실제로 렌더링할지는 정책 파일에서 결정한다.
- 새 기능 추가 시 먼저 그것이 `Expose` 정책인지, `Lift` 정책인지, 공통 렌더링 코어인지 구분하고 손대는 편이 안전하다.
- hint name은 타입명과 FNV-1a 기반 해시를 섞어서 생성한다.

## 테스트가 보장하는 것

- `Expose` 기본/명시적 모드.
- field/property/primary contract 경로.
- 다중 인터페이스, 상속 인터페이스, 제네릭 인터페이스.
- 메서드 오버로드, 제네릭 메서드, 기본값 파라미터.
- struct/record/record struct 대상.
- 이미 구현된 멤버 스킵 로직.
- base abstract member override 로직.
- `Lift`의 필터/제거/리네임/base type 옵션.
- diagnostic `MAID0002`, `MAID0003`, `MAID0004`, `MAID0005`.
- 이벤트 생성 및 이벤트 관련 override/explicit/skip 로직.

## 현재 확인된 상태

- `dotnet test InterfaceDelegation.sln --no-restore`
  - 결과: `Passed 40/40`
- 샌드박스에서는 `dotnet test`가 로컬 SDK 경로 접근 제한 때문에 실패할 수 있었다.
  - 필요한 경우 권한 상승으로 재실행하면 통과한다.
- 최근 리팩토링으로 생성기 구조가 크게 정리되었다.
  - attribute/context 분리
  - 멤버 선택 분리
  - 렌더링 분리
  - 실행 컨텍스트 분리
  - `Expose`/`Lift` 생성 경로 분리
  - 정책 로직 분리
  - 렌더링 정책 분리
  - 지원 타입 이름 정리
- 최근 추가된 진단
  - `MAID0004`: `Lift` 문자열 옵션이 존재하지 않는 멤버명을 가리킬 때 경고.
  - `MAID0005`: `Expose` 대상 타입이 인터페이스 멤버를 구현하지 않을 때 오류.
- 현재 공용 패키지 버전
  - `1.0.9`
- 현재 기준 마지막 커밋
  - `c91dec9` `Add Lift member-name diagnostics`
  - `31ad37a` `Add Expose contract diagnostics`

## 다음 작업에서 추천하는 접근

- 동작 변경 전에는 먼저 `InterfaceDelegationGeneratorTests.cs`에서 가장 가까운 기존 테스트 패턴을 찾는다.
- 생성 규칙을 바꿀 때는 가능하면 테스트를 먼저 추가하거나 기대 문자열을 같이 갱신한다.
- 버전 관련 변경은 개별 `.csproj`보다 루트 `Directory.Build.props`를 먼저 확인한다.
- `Expose`와 `Lift`는 이제 별도 정책/렌더링 파일을 가지므로, 수정 지점을 먼저 올바른 층에 배치하는 것이 중요하다.
- `Expose` 변경은 생성 단계만 볼 것이 아니라 context 생성 단계의 선행 진단과 함께 확인해야 한다.
- 공통 기능을 수정할 때는 `DelegationMemberUtilities`나 `DelegationRenderingCore` 변경이 양쪽 경로에 모두 영향을 줄 수 있음을 항상 확인한다.
- 비교 로직을 바꾸면 `MemberComparisonHelpers`가 `Expose` 계약 검증과 기존 중복 구현 스킵 로직 양쪽에 영향을 준다.
- 문서 변경이 필요한지 `README.md`도 함께 확인한다. 특히 이벤트 지원 관련 설명은 우선 검토 대상이다.

## 자주 쓸 명령

```powershell
dotnet test InterfaceDelegation.sln --no-restore
dotnet build InterfaceDelegation.sln
dotnet pack .\InterfaceDelegation\InterfaceDelegation.csproj -c Release
git status --short
```

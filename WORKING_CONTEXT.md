# InterfaceDelegation Working Context

이 문서는 이후 작업에서 빠르게 맥락을 복구하기 위한 프로젝트 운영 메모다. 코드 변경 전에 이 문서를 먼저 확인하면 구조와 주의점을 빠르게 떠올릴 수 있다.

## 한 줄 요약

- 이 저장소는 `Expose`/`Lift` 특성을 기반으로 위임 코드를 생성하는 C# Roslyn incremental source generator 프로젝트다.
- 배포 패키지 `Macaron.InterfaceDelegation`는 실제 attribute 정의가 있는 `Core`와 소스 생성기 `Generator`를 함께 패킹한다.
- 현재 기준 테스트는 `38`개 모두 통과한다.

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
- `InterfaceDelegation.Tests`
  - NUnit 기반 생성 결과 검증 프로젝트.
  - 대상 프레임워크는 `net9.0`.
  - 생성 코드를 문자열 비교로 직접 검증한다.

## 핵심 파일

- [InterfaceDelegationGenerator.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\InterfaceDelegationGenerator.cs)
  - 속성 탐지, generation context 생성, 멤버별 코드 생성, source output 등록까지 전부 담당하는 중심 파일.
- [MemberComparisonHelpers.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\MemberComparisonHelpers.cs)
  - 이미 구현된 멤버인지 판별하는 비교 로직.
  - 암시적 구현과 명시적 구현을 분리해서 추적한다.
- [MethodSignatureGenerationHelpers.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\MethodSignatureGenerationHelpers.cs)
  - 파라미터 문자열, 인자 전달 문자열, 제네릭 제약, 기본값 표현식 생성.
- [SourceGenerationHelpers.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Generator\SourceGenerationHelpers.cs)
  - 파일 헤더, partial 타입 선언 문자열, hint name 생성.
- [InterfaceDelegationGeneratorTests.cs](C:\Users\Gyu\dotnetProjects\InterfaceDelegation\InterfaceDelegation.Tests\InterfaceDelegationGeneratorTests.cs)
  - 전체 생성기 계약이 모여 있는 사실상의 사양서.

## 생성기 동작 흐름

1. `Initialize`에서 attribute가 붙은 field/property/primary-constructor parameter를 후보로 찾는다.
2. `GetGenerationContexts`가 각 attribute를 읽어 `Expose` 또는 `Lift`용 context로 변환한다.
3. 잘못된 `Expose` 사용은 즉시 diagnostic으로 바꾼다.
4. containing type 기준으로 context를 묶은 뒤, 각 context에서 생성할 멤버 코드를 모은다.
5. `AddSource`가 namespace, 중첩 타입, partial 타입 래퍼를 포함한 `.g.cs`를 만든다.

## Expose 규칙

- 대상은 non-generic interface 혹은 bound generic interface여야 한다.
- attribute 첫 번째 인자를 생략하면 멤버 자신의 타입을 인터페이스 타입으로 본다.
- 적용 대상은 field, reference-type property, primary constructor parameter다.
- 값 타입 property에 `Expose`를 붙이면 `MAID0002` 진단이 발생한다.
- 같은 containing type 안에서 같은 인터페이스를 두 번 위임하면 `MAID0003` 진단이 발생한다.
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
- `Lift`는 암시적 멤버 충돌만 본다.
- 이름 변경 후 시그니처 기준으로 기존 멤버와 충돌하면 생성하지 않는다.

## 구현상 눈여겨볼 포인트

- 이벤트 위임 지원이 이미 구현되어 있다.
  - README에는 과거 경고 문구가 남아 있을 수 있으므로 실제 코드와 문서를 비교해서 판단해야 한다.
- base class에 추상 멤버가 있으면 `override`를 생성한다.
- 현재 타입 자체에 abstract 멤버가 있으면 새 구현을 만들지 않고 건너뛴다.
- property/indexer/event/method 각각 생성 방식이 조금씩 다르므로 수정 시 분기 누락에 주의해야 한다.
- `GenerateDelegationCode`가 매우 큰 편이라 기능 추가 시 회귀 범위가 넓다.
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
- diagnostic `MAID0002`, `MAID0003`.
- 이벤트 생성 및 이벤트 관련 override/explicit/skip 로직.

## 현재 확인된 상태

- `dotnet test InterfaceDelegation.sln --no-restore`
  - 결과: `Passed 38/38`
- 샌드박스에서는 `dotnet test`가 로컬 SDK 경로 접근 제한 때문에 실패할 수 있었다.
  - 필요한 경우 권한 상승으로 재실행하면 통과한다.
- 이 작업 시작 시점에 이미 작업 트리가 더러워져 있었다.
  - 수정됨: `InterfaceDelegation.Generator/SourceGenerationHelpers.cs`
  - 수정됨: `InterfaceDelegation.Tests/InterfaceDelegation.Tests.csproj`
  - 신규 파일: `InterfaceDelegation.Tests/Test.cs`
- 위 파일들은 이번 분석 작업에서 건드리지 않았다.

## 다음 작업에서 추천하는 접근

- 동작 변경 전에는 먼저 `InterfaceDelegationGeneratorTests.cs`에서 가장 가까운 기존 테스트 패턴을 찾는다.
- 생성 규칙을 바꿀 때는 가능하면 테스트를 먼저 추가하거나 기대 문자열을 같이 갱신한다.
- `Expose`와 `Lift`는 멤버 선택 규칙이 다르므로 한쪽 수정이 다른 쪽에 어떤 영향을 주는지 항상 같이 본다.
- 문서 변경이 필요한지 `README.md`도 함께 확인한다. 특히 이벤트 지원 관련 설명은 우선 검토 대상이다.

## 자주 쓸 명령

```powershell
dotnet test InterfaceDelegation.sln --no-restore
dotnet build InterfaceDelegation.sln
dotnet pack .\InterfaceDelegation\InterfaceDelegation.csproj -c Release
git status --short
```

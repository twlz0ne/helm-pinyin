Feature: helm pinyin

  Scenario: find non-pinyin files
    Given I have files:
        | filename | content |
        | foo      | 111     |
        | bar      | 222     |
        | quux     | 333     |
    When I execute helm-ff with " bar" input and "RET" press
    Then I should in buffer "bar"
    Then I should see "222"

  Scenario: find non-pinyin files
    Given I have files:
        | filename | content |
        | foo      | 111     |
        | bar      | 222     |
        | quux     | 333     |
    When I execute helm-ff with "qu x" input and "RET" press
    Then I should in buffer "quux"
    Then I should see "333"

  Scenario: find pinyin files
    Given I have files:
        | filename   | content |
        | 文件一     | 111     |
        | 文件二     | 222     |
        | 文件三     | 333     |
    When I execute helm-ff with "wje" input and "RET" press
    Then I should in buffer "文件二"
    Then I should see "222"

  Scenario: find pinyin files
    Given I have files:
        | filename   | content |
        | 文件一     | 111     |
        | 文件二     | 222     |
        | 文件三     | 333     |
    When I execute helm-ff with "w js" input and "RET" press
    Then I should in buffer "文件三"
    Then I should see "333"

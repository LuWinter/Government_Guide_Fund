### 一、预处理

1. identify-corporation  
    确定分析样本  
    输出：identifier
2. gov-guide-fund  
    匹配引导基金持股信息  
    输出：merged_Big10SH_GGF
    merged_Big10SH_GGF_nodupl
3. accounting-conservatism  
    计算会计稳健性  
    输出：accounting_conservatism
4. control-variables  
    计算控制变量  
    输出：control_variables  
   除了3和4依赖1，2、3、4是数据独立的

### 二、实证分析

1. descriptive-table  
    描述性统计
2. basic-test  
    主检验
3. mechanism-test  
    机制检验

### 三、稳健性检验

1. hold-ratio  
    用持股比例代替是否持股
2. other-model
   
   使用其他会计稳健性模型检验
3. PSM-DID  
    1:1倾向性得分匹配引导基金持股样本，再进行DID
4. placebo-test
   
   安慰剂检验模拟5000次处理组样本

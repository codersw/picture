package com.mango.picture.model.ots;

import com.alicloud.openservices.tablestore.model.ColumnValue;
import com.alicloud.openservices.tablestore.model.PrimaryKeyValue;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.SuperBuilder;
import java.util.Map;

@EqualsAndHashCode(callSuper = true)
@Data
@SuperBuilder
public class TableStoreRow extends TableStore {

    private Map<String, PrimaryKeyValue> primaryKeyValue;

    private Map<String, ColumnValue> columnValue;
}

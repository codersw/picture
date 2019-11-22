package com.mango.picture.model.ots;

import com.alicloud.openservices.tablestore.model.DefinedColumnSchema;
import com.alicloud.openservices.tablestore.model.PrimaryKeySchema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.List;


@Data
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TableStore {

    private String tableName;

    private List<PrimaryKeySchema> primaryKeySchemas;

    private List<DefinedColumnSchema> definedColumnSchemas;
}

package com.mango.photoalbum.annotation;


import com.mango.photoalbum.constant.PermissionConst;
import java.lang.annotation.*;

/**
 * 自定义权限注解
 */
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
public @interface RequiredPermission {
    String value() default PermissionConst.NOSUPPERUSERFLAGENUM;
}

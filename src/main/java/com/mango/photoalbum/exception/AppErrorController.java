package com.mango.photoalbum.exception;

import com.mango.photoalbum.enums.ResultCodeEnum;
import com.mango.photoalbum.model.Result;
import com.mango.photoalbum.model.ResultGenerator;
import org.springframework.boot.web.servlet.error.ErrorController;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import springfox.documentation.annotations.ApiIgnore;

@RestController
@ApiIgnore
public class AppErrorController implements ErrorController {

    private static final String PATH = "/error";

    @RequestMapping(value = PATH)
    public Result error() {
        return ResultGenerator.genResult(ResultCodeEnum.NOT_FOUND);
    }

    @Override
    public String getErrorPath() {
        return PATH;
    }
}

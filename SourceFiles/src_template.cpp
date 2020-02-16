// Compiler from Wolfram Language to C++
// 
// Copyright 2019 Tianhuan Lu
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#include "librarylink.h"

wl::random_engine wl::global_random_engine;
WolframLibraryData wl::librarylink::lib_data;
WolframCompileLibrary_Functions wl::librarylink::lib_functions;
std::unique_ptr<void*[]> wl::librarylink::kernel_fptrs;

volatile bool wl::librarylink::global_abort_in_progress;
volatile bool wl::librarylink::global_stop_check_abort;
std::unique_ptr<std::thread> abort_thread;

EXTERN_C DLLEXPORT mint WolframLibrary_getVersion() {
    return WolframLibraryVersion;
}

inline void `funcid`_load_kernel_fptrs()
{
    if (wl::librarylink::kernel_fptrs)
        return;
    static const char* kernel_func_names[] = {
        `kernelfuncnames`
    };
    constexpr auto n_funcs = sizeof(kernel_func_names) / sizeof(const char*);
    wl::librarylink::kernel_fptrs = std::make_unique<void*[]>(n_funcs);
    for (size_t i = 0; i < n_funcs; ++i) {
        wl::librarylink::kernel_fptrs[i] = wl::librarylink::lib_functions->
            getExpressionFunctionPointer(
                wl::librarylink::lib_data, kernel_func_names[i]);
    }
}

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData lib_data) {
#if defined(WL_NO_RANDOM_DEVICE)
    wl::global_random_engine.seed(wl::utils::_get_time());
#else
    std::random_device rd;
    wl::global_random_engine.seed(rd());
#endif
    wl::librarylink::lib_data = lib_data;
    wl::librarylink::lib_functions =
        wl::librarylink::lib_data->compileLibraryFunctions;
    wl::librarylink::global_stop_check_abort = false;
    `funcid`_load_kernel_fptrs();
    return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize() {
    wl::librarylink::stop_check_abort(abort_thread);
    wl::librarylink::lib_data = nullptr;
}
`funcbody`

EXTERN_C DLLEXPORT int `funcid`_type(WolframLibraryData lib_data,
mint argc, MArgument *argv, MArgument res) {
using ReturnType = wl::remove_cvref_t<
    decltype(main_function(`argsv`))>;
mint type_id = wl::librarylink::get_return_type_id<ReturnType>();
MArgument_setInteger(res, type_id);
return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT int `funcid`_func(WolframLibraryData lib_data,
mint argc, MArgument *argv, MArgument res) {
    try {
        wl::librarylink::start_check_abort(abort_thread, lib_data->AbortQ);
        auto val = main_function(
            `args`
        );
        wl::librarylink::set(res, val);
    }
    catch (const std::logic_error& error) {
        wl::librarylink::send_error(error.what());
        wl::librarylink::stop_check_abort(abort_thread);
        return LIBRARY_FUNCTION_ERROR;
    }
    catch (const std::bad_alloc& error) {
        wl::librarylink::send_error(error.what());
        wl::librarylink::stop_check_abort(abort_thread);
        return LIBRARY_MEMORY_ERROR;
    }
    catch (int error)
    {
        wl::librarylink::send_error(std::string(WL_ERROR_LIBRARYLINK));
        wl::librarylink::stop_check_abort(abort_thread);
        return error;
    }
    catch (...) {
        wl::librarylink::send_error(std::string(WL_ERROR_INTERNAL));
        wl::librarylink::stop_check_abort(abort_thread);
        return LIBRARY_FUNCTION_ERROR;
    }
    return LIBRARY_NO_ERROR;
}

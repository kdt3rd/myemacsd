;;; init-project-settings.el --- cmake integration -*- lexical-binding: t -*-

;;  Copyright (c) 2024 Kimball Thurston
;;  SPDX-License-Identifier: MIT

;; can use .dir-locals.el to set values, but that is not always
;; useful when wanting to have particular projects replicated
;;
(dir-locals-set-class-variables
 'openexr-directory
 '((nil . ((projectile-project-name . "OpenEXR")
;;           (flycheck-clang-include-path
;;            .
;;            (mapcar (lambda (p)
;;                      (expand-file-name p (projectile-project-root)))
;;                    (list
;;                     "build.release/cmake"
;;                     "src/lib"
;;                     "src/lib/OpenEXRCore"
;;                     "src/lib/OpenEXR")))
;;           (flycheck-gcc-include-path . flycheck-clang-include-path)
           (cmake-ide-build-dir . "/home/kimball/Development/OSS/OpenEXR/kdt3rd/build.release")
           (cmake-ide-project-dir . "/home/kimball/Development/OSS/OpenEXR/kdt3rd")
           (cmake-ide-cmake-args  . ("-G" "Ninja"
                                     "-DCMAKE_BUILD_TYPE=Release"
                                     "-DCMAKE_INSTALL_PREFIX=/tmp/overlay"
                                     "-DOPENEXR_FORCE_INTERNAL_DEFLATE=ON"
                                     "-DOPENEXR_DEFLATE_TAG="
                                     "-DIMATH_REPO=/home/kimball/Development/OSS/Imath/kdt3rd"
                                     "-DOPENEXR_FORCE_INTERNAL_IMATH=ON"
                                     "-DIMATH_REPO=/home/kimball/Development/OSS/Imath/kdt3rd"
                                     )))))
 )

(dir-locals-set-directory-class
   "/home/kimball/Development/OSS/OpenEXR" 'openexr-directory)

(put 'projectile-project-name 'safe-local-variable (lambda (xx) t))
(put 'cmake-ide-build-dir 'safe-local-variable (lambda (xx) t))
(put 'cmake-ide-project-dir 'safe-local-variable (lambda (xx) t))
(put 'cmake-ide-cmake-args 'safe-local-variable (lambda (xx) t))
;;(put 'flycheck-clang-include-path 'safe-local-variable (lambda (xx) t))
;;(put 'flycheck-gcc-include-path 'safe-local-variable (lambda (xx) t))

(provide 'init-project-settings)

;;; init-project-settings.el ends here

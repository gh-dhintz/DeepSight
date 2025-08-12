if (all_ui_ready) {
          # UI is ready, apply preset data for this plot type
          for (i in 1:n_plots) {
            # Update X variables
            x_param <- paste0(plot_type, "_x", i)
            if (x_param %in% names(preset_data) && !is.na(preset_data[[x_param]])) {
              updateSelectInput(session, x_param, selected = preset_data[[x_param]])
            }
            
            # Update annotation inputs if this plot type has them
            if (config$has_annotations) {
              for (j in 1:config$num_annotations) {
                # The parameter naming in the preset data needs to be checked
                ann_param <- paste0(plot_type, "_annotation_", i, "_", j)
                
                if (ann_param %in% names(preset_data) && !is.na(preset_data[[ann_param]])) {
                  updateTextInput(session, ann_param, value = preset_data[[ann_param]])
                }
              }
            }
          }
          
          # Update shared notes
          notes_param <- paste0(plot_type, "_Notes_shared")
          if (notes_param %in% names(preset_data) && !is.na(preset_data[[notes_param]])) {
            updateTextAreaInput(session, notes_param, value = preset_data[[notes_param]])
          }
          
          # Mark this plot type as processed
          processed_plots <- c(processed_plots, plot_type)
        }
      } else {
        # If n_plots is 0 or NULL, still mark as processed
        processed_plots <- c(processed_plots, plot_type)
      }
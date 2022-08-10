import torch
from simpletransformers.classification import ClassificationModel, ClassificationArgs
import os
import pandas as pd
import random


from sklearn.model_selection import train_test_split
os.environ["TOKENIZERS_PARALLELISM"] = "false"

def py_detect_cuda():
    return(torch.cuda.is_available())

def py_train(data, num_labels, output_dir, best_model_dir, cache_dir, model_type, model_name, num_train_epochs, train_size, manual_seed, regression, verbose):
    if py_detect_cuda():
        torch.cuda.empty_cache()
    random.seed(manual_seed)
    data.columns = ["text", "labels"]
    if train_size < 1 and num_train_epochs <= 4:
        num_train_epochs = 20
    mod_args = {
        'reprocess_input_data': True, 
        'overwrite_output_dir': True,
        'fp16': True,
        'output_dir': output_dir,
        "best_model_dir": best_model_dir,
        "cache_dir": cache_dir,
        "use_multiprocessing": False,
        "use_multiprocessing_for_evaluation": False,
        "save_steps": -1,
        "save_eval_checkpoints": False,
        "save_model_every_epoch": False,
        "num_train_epochs": num_train_epochs,
        "manual_seed": manual_seed,
        "silent": not verbose
    }
    if regression:
        mod_args["regression"] = True
    if train_size < 1:
        mod_args["use_early_stopping"] = True
        mod_args["evaluate_during_training"] = True
        mod_args["early_stopping_delta"] = 0.02
        mod_args["early_stopping_patience"] = 1
        if regression:
            mod_args["early_stopping_metric"] = "eval_loss"
        else:
            ## Classification
            mod_args["early_stopping_metric"] = "mcc"
            mod_args["early_stopping_metric_minimize"] = False
        model = ClassificationModel(model_type = model_type, model_name = model_name, num_labels = num_labels, use_cuda = py_detect_cuda(), args = mod_args)
        data_train, data_cv = train_test_split(data, train_size = train_size, stratify = data['labels'].values.tolist())
        model.train_model(data_train, eval_df = data_cv, verbose = verbose, show_running_loss = verbose)
    else:
        model = ClassificationModel(model_type = model_type, model_name = model_name, num_labels = num_labels, use_cuda = py_detect_cuda(), args = mod_args)
        model.train_model(data, verbose = verbose, show_running_loss = verbose)

def py_predict(to_predict, model_type, output_dir, return_raw = False):
    if len(to_predict) == 1:
        to_predict = [to_predict]
    model = ClassificationModel(model_type, output_dir, args = {
        'reprocess_input_data': True,
        "use_multiprocessing": False,
        "fp16": True,
        "use_multiprocessing_for_evaluation": False
    })
    predictions, raw_outputs = model.predict(to_predict)
    if return_raw:
        return(raw_outputs)
    else:
        return(predictions)
